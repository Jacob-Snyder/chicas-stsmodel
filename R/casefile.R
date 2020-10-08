# .col_spec_csv = "dccccccccccdDDD"
# .col_spec_xlsx = c("numeric",
#                    rep("text", 10),
#                    "numeric",
#                    "date",
#                    "date",
#                    "date")
# 
# excel_date <- function (x, origin = "1900-01-01", ...) {
#     ## from openxlsx::convertToDate
#     x <- as.numeric(x)
#     notNa <- !is.na(x)
#     if (origin == "1900-01-01") {
#         x[notNa] <- x[notNa] - 2
#     }
#     return(as.Date(x, origin = origin, ...))
# }
# 
# column_convertors_xlsx <- function(){
#     ## all columns remain as text except...
#     list(
#         "FINALID"=as.numeric,
#         "age" = as.numeric,
#         "Onsetdate" = excel_date,
#         "specimen_date" = excel_date,
#         "lab_report_date" = excel_date
#     )
# }
# 
# csvdate <- function(x){
#     as.Date(x,format="%m/%d/%y")
#     
# }
# 
# column_convertors_csv <- function(){
#     list(
#         "Onsetdate" = csvdate,
#         "specimen_date" = csvdate,
#         "lab_report_date" = csvdate)
# }
# 
read_casefile <- function(casefile){
#     if(grepl("\\.xlsx$",casefile, ignore.case=TRUE)){
#         cases  <- readxl::read_xlsx(casefile, col_types="text")
#         column_convertors = column_convertors_xlsx()
#     }else if(grepl("\\.csv$",casefile, ignore.case=TRUE)){
#         cases <- read.csv(casefile, stringsAsFactors=FALSE)
#         column_convertors = column_convertors_csv()
#     }else{
#         stop("Unrecognised case file extension for ",casefile," - not CSV or XLSX")
#     }
#     cc = column_convertors
#     for(col in names(cc)){
#         cases[[col]] = cc[[col]](cases[[col]])
#     }
    cases <- read.csv(casefile, stringsAsFactors=FALSE)
    return(cases)
 }


