required_inputs <- function(){
    list(cases=
             list(
                 description="line listing",
                 extension=c("xlsx","csv"),
                 restricted=TRUE,
                 reader=readr::read_csv
             ),
         
         # flows =
         #     list(
         #         description="Area-area population flow",
         #         extension="rds",
         #         restricted=FALSE,
         #         reader=readRDS
         #     ),
          policy =
              list(     # stringency and testing
                  description="policy",
                 extension="csv",
                  restricted=TRUE,
                  reader=readr::read_csv
              ),
         areas =
             list(
                 description="Geodata for areas",
                 extension="gpkg",
                 restricted=FALSE,
                 reader=st_read
             )
         )     
}

# read_traffic <- function(f){
#     read_xlsx(f, skip=4)
# }


read_all <- function(...){
    namelist = list(...)
    required = required_inputs()
    given = names(namelist)
    missing_names = setdiff(names(required), given)
    if(length(missing_names)>0){
        message("Required file arguments missing: ",paste0(missing_names, collapse=":"))
        stop()
    }
    extra_names = setdiff(given, names(required))
    if(length(extra_names) > 0){
        warning("Extra file arguments present: ",paste0(extra_names, collapse=":"))
    }
    OUT=list()
    for(n in names(namelist)){
        if(!n %in% names(required)){
            stop('"',n, '" not found in required input names: ', paste0(names(required),collapse=":"))
        }
        entry = required[[n]]
        message("Reading ",entry$description)
        OUT[[n]] = entry$reader(namelist[[n]])
    }
    attr(OUT,"meta") = source_metadata(unlist(namelist))
    class(OUT) <- c("alldata","list")
    OUT
}

print.alldata <- function(x,...){
    print(attr(x,"meta"))
    invisible(0)
}

write_metadata <- function(ad, path){
    write.csv(attr(ad,"meta"),path, row.names=FALSE)
}

source_metadata <- function(paths){
    meta = data.frame(name=names(paths))
    meta$given=paths
    meta$fullpath = normalizePath(paths)

    infos = file.info(paths)
    meta$mtime = infos$mtime
    meta$md5 = tools::md5sum(paths)
    meta
}


    
    
