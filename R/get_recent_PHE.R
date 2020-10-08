get_recent_line_listing <- function(root){
    ## find the most recent Line listing file from `root` folder
    dated_folders = list.files(root, pattern=".*20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]$", recursive=FALSE, full.names=TRUE,include.dirs=TRUE)
    last_folder = sort(dated_folders, decreasing=TRUE)[1]
    xlsx_or_csv = Sys.glob(file.path(last_folder, "Anon*Line*.*"))
    return(xlsx_or_csv)
}

get_recent_transport <- function(root){
    ## find the most recent Dft data
    dated_folder_pattern = file.path(root,"*")
    all_xlsx = Sys.glob(file.path(file.path(dated_folder_pattern, "*_COVID19_road_traffic_national_table.xlsx")))
    latest_xlsx = sort(all_xlsx, decreasing=TRUE)[1]
    return(latest_xlsx)
}
