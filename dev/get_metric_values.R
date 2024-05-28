#get a list of data frames containing all values from the manually downloaded spreadsheet

#the spreadsheet was downloaded from this url:
#https://assets.publishing.service.gov.uk/media/65c60e83cc433b000ca90b32/The_Statutory_Biodiversity_Metric_Calculation_Tool_-_Macro_disabled_02.24.xlsx

path <- "C:/Users/euan.mckenzie/Downloads/The_Statutory_Biodiversity_Metric_Calculation_Tool_-_Macro_enabled.xlsx"
library(readxl)
sheets <- excel_sheets(path)

#read in all the data from the spreadsheet
metric_sheets <- lapply(sheets, function(sheet){
  read_excel(path, sheet = sheet)
})
names(metric_sheets) <- sheets

#save data to a file in the repo
usethis::use_data(metric_sheets, overwrite = TRUE)
