library(EDCReport)
library(RODBC)
library(xlsx)
library(lattice)
library(reshape2)
library(dplyr)

source("S:/EDC/2011 Data Collection/Business Rules/connectioninfo.r")

setwd("R:/Confidential/Formal Data Requests/Johnson 2016")

options(stringsAsFactors = F)


################################################################################
# 
# Pulling the species codes for Beth
#
# these are all of the species codes in pacfin by year and gear type (trawl or
# fixed gear)
#
################################################################################

spcodesbyyear <- sqlQuery(ifqpub, "
select distinct 
case when edcspid = 'Petrale sole' then 'Petrale sole'
     when edcmgrp = 'DTS' then edcspid
     else edcmgrp end as spgrp, 
case when edcgrid = 'TWL' then 'Trawl' else 'Fixed gear' end as gear, 
e.spid, cname, complex, complex2, complex3, sname, e.year
from edcfish_mv e, ftidfishery f, pacfin.sp@pacfin sp
where e.agid = f.agid and e.ftid = f.ftid and e.year = f.year and e.spid = sp.spid and e.mgrp = 'GRND'
  and e.year between 2009 and 2014 and (fisherycode like 'FTDTS' or fisherycode like 'TTDTS%')")


################################################################################
# 
# Protecting confidential information
#
################################################################################

#-------------------------------------------------------------------------------
# vessel counts
#-------------------------------------------------------------------------------



vss <- sqlQuery(
  ifqpub, "
  select sum(rev) rev, sum(lbs) lbs, vessel_id, iopcid, e.year, 
  case when edcspid = 'Petrale sole' then edcspid
       when edcmgrp = 'DTS' then edcspid else edcmgrp end as spgrp,
  case when edcgrid in ('TWL', 'MDT', 'SEINE') then 'Trawl' else 'Fixed gear' end as gear
  from edcfish_mv e, ftidfishery f
  where e.agid = f.agid and e.ftid = f.ftid and e.year = f.year
    and mgrp = 'GRND'
    and e.year between 2009 and 2014 
    and iopcid <> 'Santa Barbara' 
    and (fisherycode like 'FTDTS%' or fisherycode like 'TTDTS%') and edcgrid <> 'SEINE'
  group by case when edcspid = 'Petrale sole' then edcspid
  when edcmgrp = 'DTS' then edcspid
  else edcmgrp end, 
case when edcgrid in ('TWL', 'MDT', 'SEINE') then 'Trawl' else 'Fixed gear' end,
  vessel_id, e.year, iopcid"
)

vss$portgrp <- with(vss, 
  ifelse(IOPCID%in%c('San Francisco', 'Bodega Bay'), 'San Francisco and Bodega Bay',
    ifelse(IOPCID%in%c('Tillamook', 'Astoria'), 'Astoria and Tillamook',
      ifelse(IOPCID%in%c('Brookings', 'Crescent City'), 'Brookings and Crescent City',
        ifelse(IOPCID%in%c('Monterey', 'Morro Bay'), 'Monterey and Morro Bay',
          ifelse(IOPCID%in%c('Puget Sound', 'South and central WA coast', 'North WA coast'), 'Washington', IOPCID))))))

VSS <- DotDcast(vss, portgrp + SPGRP + YEAR + GEAR + VESSEL_ID ~ ., valvar = 'REV', sum)


# rule of 3
#-------------------------------------------------------------------------------

vssN <- dcast(VSS, portgrp + SPGRP + GEAR ~ YEAR, value.var = 'YEAR', length)

#-------------------------------------------------------------------------------
# processor count data
#-------------------------------------------------------------------------------

proc <- sqlQuery(ifqpub, "
select sum(rev) rev, sum(lbs), case when edcspid = 'Petrale sole' then edcspid
     when edcmgrp = 'DTS' then edcspid
else edcmgrp end as spgrp, 
case when edcgrid in ('TWL', 'MDT') then 'Trawl' else 'Fixed gear' end as gear, 
proc, iopcid, e.year
from edcfish_mv e, ftidfishery f
where e.agid = f.agid and e.ftid = f.ftid and e.year = f.year 
and mgrp = 'GRND'
  and e.year between 2009 and 2014
  and iopcid <> 'Santa Barbara' and (fisherycode like 'FTDTS%' or fisherycode like 'TTDTS%') 
group by case when edcspid = 'Petrale sole' then edcspid
when edcmgrp = 'DTS' then edcspid
else edcmgrp end, case when edcgrid in ('TWL', 'MDT') then 'Trawl' else 'Fixed gear' end, 
  proc, e.year, iopcid")

proc$portgrp <- with(proc, 
  ifelse(IOPCID%in%c('San Francisco', 'Bodega Bay'), 'San Francisco and Bodega Bay',
    ifelse(IOPCID%in%c('Tillamook', 'Astoria'), 'Astoria and Tillamook',
      ifelse(IOPCID%in%c('Brookings', 'Crescent City'), 'Brookings and Crescent City',
        ifelse(IOPCID%in%c('Monterey', 'Morro Bay'), 'Monterey and Morro Bay',
          ifelse(IOPCID%in%c('Puget Sound', 'South and central WA coast'), 'Washington', IOPCID))))))

PROC <- DotDcast(proc, portgrp + SPGRP + GEAR + YEAR + PROC ~ ., valvar = 'REV', sum)

# rule of 3
#-------------------------------------------------------------------------------

procN <- dcast(PROC, portgrp + SPGRP ~ YEAR, value.var = 'YEAR', length)

# 90 10 rule
#-------------------------------------------------------------------------------

proc9010 <- dcast(PROC, SPGRP + portgrp + YEAR ~ PROC, value.var = 'REV', sum)

proc9010$tot <- rowSums(proc9010[,-c(1:3)], na.rm = T)

proc9010 <- proc9010[!proc9010$tot == 0,]

percs <- melt(data.frame(proc9010[,1:3], proc9010[,-c(1:3,ncol(proc9010))]/proc9010[,ncol(proc9010)]),
  c('SPGRP', 'portgrp', 'YEAR'))

proc90 <- dcast(percs, portgrp + SPGRP ~ YEAR, value.var = 'value', fun.aggregate = function(x) ifelse(length(x) == 0, 0, max(x)))

#-------------------------------------------------------------------------------
# combining all measures together
#-------------------------------------------------------------------------------

allchx <- merge(merge(
melt(proc90, c('portgrp', 'SPGRP'), value.name = 'proc90'),
melt(procN, c('portgrp', 'SPGRP'), value.name = 'procN')),
melt(vssN, c('portgrp', 'SPGRP', 'GEAR'), value.name = 'vssN'))

allchx$keep <- with(allchx, 
  ifelse(proc90 <= .9 & !(vssN > 0 & vssN < 3) & !(procN > 0 & procN < 3),
    'keep', 'drop'))

finalrule <- subset(allchx, select = c(portgrp, SPGRP, GEAR, variable, keep)) %>%
  dplyr::rename(YEAR = variable)

# filtering the data

vssfilter <- merge(finalrule, VSS)

vssfilter$REV <- ifelse(vssfilter$keep == 'drop', -999, vssfilter$REV)


################################################################################
#
# vessel counts
#
################################################################################

vsscounts <- merge(DotDcast(unique(VSS[, c('VESSEL_ID', 'portgrp', 'SPGRP', 'YEAR')]), portgrp + SPGRP + YEAR ~ ., 
  valvar = 'VESSEL_ID', length),
  finalrule)

vsscounts$finalnum <- ifelse(vsscounts$keep == 'drop', -999, vsscounts$VESSEL_ID)


################################################################################
#
# proc counts
#
################################################################################

proccounts <- unique(PROC[, c('PROC', 'portgrp', 'SPGRP', 'YEAR')]) %>%
  DotDcast(portgrp + SPGRP + YEAR ~ ., valvar = 'PROC', length) %>%
  merge(finalrule)

proccounts$finalnum <- ifelse(proccounts$keep == 'drop', -999, proccounts$PROC)



################################################################################
#
# revenue and lbs
#
################################################################################

revlbs <- group_by(vss, SPGRP, portgrp, GEAR, YEAR, SPGRP) %>%
  summarise(
  REVENUE = sum(REV),
  LANDINGS = sum(LBS)) %>%
    merge(finalrule) %>%
  data.frame()

revlbs$REVENUE <- ifelse(revlbs$keep == 'keep', revlbs$REVENUE, -999)
revlbs$LANDINGS <- ifelse(revlbs$keep == 'keep', revlbs$LANDINGS, -999)

revenuespport <- dcast(revlbs, SPGRP + portgrp + GEAR ~ YEAR, 
  value.var = 'REVENUE', fill = 0)
landingsspport <- dcast(revlbs, SPGRP + portgrp + GEAR ~ YEAR, 
  value.var = 'LANDINGS', fill = 0)




################################################################################
#
# Assigning ports 
#
################################################################################

vssrevport <- dcast(vss, YEAR + GEAR + VESSEL_ID ~ portgrp, 
  value.var = 'REV', sum)

vssport <- data.frame(vssrevport[,1:3], portgrp = names(vssrevport[,-c(1:3)])[apply(vssrevport[,-c(1:3)], 1, which.max)])


################################################################################
#
# Vessel characteristics
#
################################################################################

vsschar.int <- sqlQuery(ifqpub, "
select d_number_response, vessel_id, survey_year as year, fullcode
from edcdata
where survey_type = 'CATCHER VESSEL'
and fullcode in ('VSSFLCAP', 'VSSHP', 'VSSLNG', 'VVMRK')")

vsschar <- dcast(vsschar.int, VESSEL_ID + YEAR ~ FULLCODE, 
  value.var = 'D_NUMBER_RESPONSE', 
  mean, na.rm = T)

mean999 <- function(x) ifelse(length(x) > 0 & length(x) < 3, -999, mean(x, na.rm = T))

vsscharport <- merge(vsschar, vssport) %>%
  group_by(YEAR, GEAR, portgrp) %>%
  dplyr::summarise(
    FuelCapacity = mean999(VSSFLCAP),
    HorsePower = mean999(VSSHP),
    Length = mean999(VSSLNG),
    'Number of vessels' = ifelse(length(YEAR) > 0 & length(YEAR) < 3, -999, length(YEAR))) %>%
  data.frame()

names(vsscharport) <- gsub("[.]", " ", names(vsscharport))


################################################################################
#
# Days, fuel use, fishing speed, crew size
#
################################################################################

ruRaw <- merge(sqlQuery(ifqpub, "
select d_number_response, vessel_id, survey_year as year,
 case when fullcode like '%DAS%' then 'Days'
      when fullcode like '%RUCW%' then 'Crew'
      when fullcode like '%RUSPD%' then 'Speed'
      when fullcode like '%RUFL%' then 'Fuel'
      else 'other' end as FULLCODE,
 case when fullcode like '%TWL%' then 'Trawl'
      when fullcode like '%FG%' then 'Fixed gear' 
      else 'Other' end as GEAR
from edcdata
where fullcode in 
('DASGRNDFG', 'DASGRNDTWL',
 'RUCWGRNDFG', 'RUCWGRNDTWL', 
 'RUSPDGRNDFG', 'RUSPDGRNDTWL',
 'RUFLGRNDFG', 'RUFLGRNDTWL')
  and d_number_response is not null and d_number_response <> 0
  and survey_type = 'CATCHER VESSEL'"),
  vssport)


ruvessels <-
  dcast(
    DotDcast(
      ruRaw[ruRaw$FULLCODE != 'Days',],
      VESSEL_ID + YEAR + GEAR + portgrp + FULLCODE ~ .,
      valvar = 'D_NUMBER_RESPONSE',
      mean,
      na.rm = T
    ),
    YEAR + GEAR + portgrp ~ FULLCODE,
    value.var = 'D_NUMBER_RESPONSE',
    mean
  ) %>%
  
  merge(dcast(
    DotDcast(
      ruRaw[ruRaw$FULLCODE == 'Days',],
      VESSEL_ID + YEAR + GEAR + portgrp + FULLCODE ~ .,
      valvar = 'D_NUMBER_RESPONSE',
      sum,
      na.rm = T
    ),
    YEAR + GEAR + portgrp ~ FULLCODE,
    value.var = 'D_NUMBER_RESPONSE',
    mean
  )) %>%  
  
  merge(vsscharport[, c('YEAR', 'GEAR', 'portgrp', 'Number of vessels')], all.x = T)

ruvessels$Crew  <- ifelse(ruvessels$'Number of vessels' == -999, -999, ruvessels$Crew)
ruvessels$Fuel  <- ifelse(ruvessels$'Number of vessels' == -999, -999, ruvessels$Fuel)
ruvessels$Speed <- ifelse(ruvessels$'Number of vessels' == -999, -999, ruvessels$Speed)
ruvessels$Days  <- ifelse(ruvessels$'Number of vessels' == -999, -999, ruvessels$Days)

ruvessels$Speed <- ifelse(ruvessels$GEAR == 'Fixed gear', NA, ruvessels$Speed)



################################################################################
#
# Cost
#
################################################################################

costsRaw <- sqlQuery(ifqpub, "select vessel_id, year, fullcode, fisherycode,
case when fisherycode like '%FT%' then 'Fixed gear' else 'Trawl' end as gear,
sum(cost) cost 
from steinerer.costs
where year <= 2014 and year > 2008 and (fisherycode like 'FTDTS%' or fisherycode like 'TTDTS%')
  group by vessel_id, year, fullcode, case when fisherycode like '%FT%' then 'Fixed gear' else 'Trawl' end, fisherycode")

costsRaw$cat <- with(costsRaw, 
  ifelse(grepl('CX', FULLCODE), 'Fixed costs', 
    ifelse(grepl('QP', FULLCODE) | grepl('LEP', FULLCODE) | grepl('QS', FULLCODE) | FULLCODE == 'EXDEPRALL', 'other',  
      ifelse(grepl('WC', FULLCODE) & !grepl('FG', FULLCODE), 'Variable costs', 'Fixed costs'))))

costs <- merge(costsRaw, vssport)

finalcosts <- subset(costs, cat != 'other') %>%
  DotDcast(YEAR + GEAR + portgrp + VESSEL_ID + cat ~ .,
    valvar = 'COST', sum) %>%
  dcast(YEAR + GEAR + portgrp ~ cat, mean, value.var = 'COST') %>%
  merge(vsscharport[,c('YEAR', 'GEAR', 'portgrp', 'Number of vessels')])

finalcosts$'Fixed costs' <- ifelse(finalcosts$'Number of vessels' == -999, -999, finalcosts$'Fixed costs')
finalcosts$'Variable costs' <- ifelse(finalcosts$'Number of vessels' == -999, -999, finalcosts$'Variable costs')


################################################################################
#
# Net Revenue
#
################################################################################

revenue <- merge(sqlQuery(ifqpub, "select vessel_id, year,
case when r.fisherycode like '%FT%' then 'Fixed gear' else 'Trawl' end as gear,
sum(rev) rev 
from revlbsdas r 
  where fisherycode like 'FTDTS%' or fisherycode like 'TTDTS%'
  group by vessel_id, year, 
  case when r.fisherycode like '%FT%' then 'Fixed gear' else 'Trawl' end")
, vssport)

costsrev <- merge(revenue, dcast(costsRaw, VESSEL_ID + YEAR ~ cat, value.var = 'COST', sum))
costsrev$vnr <- costsrev$REV - costsrev$'Variable costs'
costsrev$tnr <- costsrev$vnr - costsrev$'Fixed costs'

totrev <- group_by(costsrev, YEAR, GEAR, portgrp) %>%
  summarise(
  'TotRev' = mean(REV),
  'VarCostNetRev' = mean(vnr),
  'TotCostNetRev' = mean(tnr),
  'NumVss' = length(vnr)) %>%
  data.frame()

totrev$'TotRev'        <- ifelse(totrev$NumVss > 0 & totrev$NumVss < 3, -999, totrev$'TotRev')
totrev$'VarCostNetRev' <- ifelse(totrev$NumVss > 0 & totrev$NumVss < 3, -999, totrev$'VarCostNetRev')
totrev$'TotCostNetRev' <- ifelse(totrev$NumVss > 0 & totrev$NumVss < 3, -999, totrev$'TotCostNetRev')



################################################################################
#
# Notes
#
################################################################################

notes <- data.frame(
  sheet = c('overall', 'specieslist', 'Revenue', 'Landings', 'VesselCharacteristics', 'dayscrewfuelspeeddays', 'Costs', 'NetRevenue'),
  notes = c(
    "-999 denotes cases that have been suppressed to protect confidential data. The revenue and landings by species tables are summarized at the port where the fish were landed. For the other tables (vessel specific info) are summarized at the port where the largest revenue was earned by each vessel. NA denotes not applicable (e.g. speed while fishing with fixed gear).",
    "The SPGRP is the list of species groups I use for all output. For your information, I've tacked on a bunch of species info from PacFIN, for more info, you can go to their website: http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp_tree.txt or see table 'sp': http://pacfin.psmfc.org/pacfin_pub/table_cols.php",
    "Total revenue by landings port and species group ($)",
    "Total landings by landings port and species group (lbs)",
    "Average vessel characteristics by port. Vessels are assigned to the port where they earned the highest ex-vessel revenue for that year. Vessel fuel capacity is measured in gallons and vessel length.",
    "Average number of crew (not including captain) per boat, total number of days in fishery, average fuel use per day (gallons) and average speed (knots). Vessels are assigned to the port where they earned the highest ex-vessel revenue for that year.",
    "Average fixed and variable costs by year, gear type and port group. Vessels are assigned to the port where they earned the highest ex-vessel revenue for that year.",
    "Average variable cost net revenue and total cost net revenue by year, gear type and port group. Vessels are assigned to the port where they earned the highest ex-vessel revenue for that year."))



variabledefs <- data.frame(
  variable = c('SPGRP', 'GEAR', 'YEAR', 'portgrp', 'FuelCapacity', 'HorsePower', 'Length', 'Crew', 'Fuel', 'Speed', 'Days', 'Variable costs', 'Fixed costs', 'TotRev', 'VarCostNetRev', 'TotCostNetRev'),
  definition = c('Species group, see specieslist sheet for the list of species included in each group by year',
    'Gear type - trawl includes both bottom and midwater trawl, fixed gear includes both pot and longline gear',
    'Year - year in which the activity occurred',
    'portgrp - the port group',
    'Fuel capacity - fuel capacity of the vessel in gallons',
    'Horsepower - horsepower of vessel main engine',
    'Vessel length (ft)',
    'Number or crew - does not include captain',
    'Fuel - average fuel use per day (gallons)',
    'Speed - average speed while fishing (knots)',
    'Days at sea - number of days spent fishing, participants are instructed to count partial days as full days',
    'Variable costs include expenses on:  Bait, Captain, Communications, Crew, Fishing association dues, Food, Freight, Fuel and lubrication, Ice, License fees, Observer costs, Offloading, Supplies, Travel, and Trucking', 
    'Fixed costs include expenses on: Processing equipment, Vessel and on-board equipment, Insurance premium payments, and Moorage',
    'Total revenue from fishing in the IFQ fisheries',
    'Variable cost net revenue - total revenue from fishing minus variable costs (see definition of variable costs above)',
    'Total cost net revenue - total revenue from fishing minus variable and fixed costs (see definition of costs above)'))



writetoexcel = T

if(writetoexcel == T) {
# Saving everything ####

write.xlsx(spcodesbyyear, sheetName = 'specieslist', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = F)

write.xlsx(notes, sheetName = 'Notes', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(variabledefs, sheetName = 'VariableDefinitions', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(vsscharport, sheetName = 'VesselCharacteristics', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(dcast(vsscounts, portgrp + SPGRP + GEAR ~ YEAR, 
  value.var = 'finalnum'), sheetName = 'vesseldeliverycount', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(dcast(proccounts, portgrp + SPGRP + GEAR ~ YEAR, 
  value.var = 'finalnum'), sheetName = 'fishbuyercount', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(ruvessels, sheetName = 'dayscrewfuelspeeddays', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(revenuespport, sheetName = 'Revenue', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(landingsspport, sheetName = 'Landings', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(finalcosts, sheetName = 'Costs', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

write.xlsx(totrev[,-ncol(totrev)], sheetName = 'NetRevenue', 
  file = 'econ4osuonlysablefish2009-2014.xlsx', row.names = F, append = T)

}