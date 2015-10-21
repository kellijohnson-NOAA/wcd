create_modelstructures <- function() {
modelStructures <- list()
# Model 1 only strata and year effects estimated, vessel-years as random
# no strata-yr interactions
modelStructures[[1]] <- list("StrataYear.positiveTows" = "zero",
                             "VesselYear.positiveTows" = "random",
                             "StrataYear.zeroTows"     = "zero",
                             "VesselYear.zeroTows"     = "random",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 2, fixed StrataYear interactions, vessel-years as random
# mimics J. Wallace 2011 GLMM code
modelStructures[[2]] <- list("StrataYear.positiveTows" = "fixed",
                             "VesselYear.positiveTows" = "random",
                             "StrataYear.zeroTows"     = "fixed",
                             "VesselYear.zeroTows"     = "random",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 3, all random interactions,
# default values from bayesGLM Writeup 2.4.pdf,
# it will take longer to run with all random effects for interactions
modelStructures[[3]] <- list("StrataYear.positiveTows" = "random",
                             "VesselYear.positiveTows" = "random",
                             "StrataYear.zeroTows"     = "random",
                             "VesselYear.zeroTows"     = "random",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 4, only strata and year effects, no interactions
modelStructures[[4]] <- list("StrataYear.positiveTows" = "zero",
                             "VesselYear.positiveTows" = "zero",
                             "StrataYear.zeroTows"     = "zero",
                             "VesselYear.zeroTows"     = "zero",
                             "Catchability.positiveTows" = "one",
                             "Catchability.zeroTows"   = "zero",
                             "year.deviations"         = "fixed",
                             "strata.deviations"       = "fixed",
                             "Vessel.positiveTows"     = "zero",
                             "Vessel.zeroTows"         = "zero")

# Model 5, correlated positive components for strata-year and vesselyear interactions
modelStructures[[5]] <-  list("StrataYear.positiveTows" = "correlated",
                              "VesselYear.positiveTows" = "correlated",
                              "StrataYear.zeroTows"     = "correlated",
                              "VesselYear.zeroTows"     = "correlated",
                              "Catchability.positiveTows" = "one",
                              "Catchability.zeroTows"   = "zero",
                              "year.deviations"         = "fixed",
                              "strata.deviations"       = "fixed",
                              "Vessel.positiveTows"     = "zero",
                              "Vessel.zeroTows"         = "zero")
return(modelStructures)
}
