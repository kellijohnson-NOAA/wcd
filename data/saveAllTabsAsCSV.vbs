'* A script to open an excel file and save each worksheet as an individual csv file.
'* run via command line or system() in R with cscript sript.vbs filename.xls
'* This will only work with .xls files not other extensions, you can modify the replace
'* call below if you wish to use a different type of excel file.

Option Explicit

Const xlXMLSpreadsheet = 46
Const xlCSV = 6

'* Declare variables
Dim args, objFSO, theext, xl, wb, ws

'* Set the input arguments used on the command line to args
'* If there is more than one or less than one argument produce an error
Set args = WScript.Arguments
If args.Count <> 1 Then
  WScript.Echo "Syntax: cscript " & WScript.ScriptName & " filename"
  WScript.Quit(1)
End If

'* Get the folder name
Set objFSO = CreateObject("Scripting.FileSystemObject")

'* Determine if an xlsx file (theext > 0) or an xls file (theext = 0)
'* to be used later in the code
theext = Instr(args(0), ".xlsx")

'* Create an excel object that can be opened in the following line
Set xl = CreateObject("Excel.Application")
Set wb = xl.Workbooks.Open(objFSO.GetAbsolutePathName(args(0)))

'* For each excel worksheet save it with the parent name _ worksheet name
xl.DisplayAlerts = TRUE
For Each ws In wb.Worksheets
 ws.activate
 If theext > 0 Then
     wb.SaveAs Replace(objFSO.GetAbsolutePathName(args(0)), ".xlsx", "_") & Replace(ws.Name, " ", "_") & ".csv", xlCSV
   Else
     wb.SaveAs Replace(objFSO.GetAbsolutePathName(args(0)), ".xls", "_") & Replace(ws.Name, " ", "_") & ".csv", xlCSV
 End If
Next

wb.Close False
xl.Quit
WScript.Quit