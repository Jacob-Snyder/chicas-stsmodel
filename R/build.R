
op = function(outputdir){
    function(n){
        file.path(outputdir, n)
    }
}

buildmap <- function(outputdir){
    F = op(outputdir)
    pf = read.csv(F("pred_forecast.csv"))
    epg = read.csv(F("ex_prob_gr.csv"))
    ltla = st_read(F("data/processed/geodata/ltla.gpkg"))
    md = mapdata(ltla, epg, "lad19cd","lad19nm", pf)
    map = exmap(md,imagefolder=file.path(getwd(),F("output/time_series/")))
    map

}

update_day <- function(outputdir, days){
    F = op(outputdir)
    map = buildmap(outputdir)
    cases = read.csv(F("coronavirus-cases_latest.csv"))
    lastcase = max(as.Date(cases$Specimen.date))
    if(!grepl("^\\d\\d\\d\\d-\\d\\d-\\d\\d$", lastcase)){
        stop("Malformed last date. Not deleting the folder")
    }
    dpath = normalizePath(file.path(days, as.character(lastcase)), mustWork=FALSE)
    message("dpath is ",dpath)
    if(file.exists(dpath)){
        unlink(dpath, recursive=TRUE)
    }
    dir.create(dpath)
    dpath = normalizePath(dpath)
    mappath = normalizePath(file.path(dpath,"map.html"), mustWork=FALSE)
    message("saving map to ",mappath)
    saveWidget(map, mappath)
}
