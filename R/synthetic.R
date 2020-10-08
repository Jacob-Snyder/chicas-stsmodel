## To run a synthetic example:

synth_and_run <- function(datadir, nregions=12){
    make_sample_data(datadir, nregions=nregions)
    run_from(datadir)
}

run_from <- function(datadir){
    fp = function(name){file.path(datadir, name)}
    sdata = read_all(cases=fp("linelisting.xlsx"),
                     flows=fp("flows.rds"),
                     traffic = fp("traffic.xlsx"),
                     areas=fp("regions.gpkg"))
    model_and_map(sdata, datadir, useDate=TRUE)
}


make_traffic <- function(startdate, enddate, min=80, max=110){
    trafficnames = c("Cars","Light commercial vehicles","Heavy goods vehicles",
                     "All motor vehicles")
    startdate = as.Date(startdate)
    enddate = as.Date(enddate)
    dates = seq(startdate, enddate, by=1)
    nrow = length(dates)
    df = data.frame(Date=dates)
    for(n in trafficnames){
        df[[n]] = round(runif(nrow, min=min, max=max))
    }
    df
}

write_traffic <- function(trafficdata, xlsxfile, startRow=6){
    docs = data.frame(doc=c("*Simulated* Change in daily traffic volumes on Great Britain's roads",
             "The table below are random numbers pretending to be the volume of traffic on Britain's roads",
             "Index: 100 = a random baseline",
             "Source: random numbers"))

    wb = openxlsx::createWorkbook()
    addWorksheet(wb,"Table")
    writeDataTable(wb,"Table",trafficdata,startRow=startRow)
    writeData(wb,"Table",docs,startRow=1,colNames=FALSE)
    saveWorkbook(wb, xlsxfile, overwrite=TRUE)
}


make_poisson_case_counts <- function(start, end, dailytotal){
    start = as.Date(start)
    end = as.Date(end)
    d = data.frame(Date = seq(start, end, 1))
    d$count = rpois(nrow(d), dailytotal)
    d    
}

make_binom_case_count <- function(start, end, rate){

}

make_linelisting <- function(regions, dailycasecounts, cd="lad19cd", nm="lad19nm"){
    ## FINALID
    ## pillar
    ## NHSER_code
    ## NHSER_name
    ## PHEC_code
    ## PHEC_name
    ## UTLA_code
    ## UTLA_name
    ## LTLA_code ** Used
    ## LTLA_name
    ## sex
    ## age
    ## Onsetdate
    ## specimen_date ** Used
    ## lab_report_date

    fulldates = rep(dailycasecounts$Date, dailycasecounts$count)
    regs = sample(1:nrow(regions), length(fulldates), replace=TRUE)
    caselisting = data.frame(LTLA_code = regions[[cd]][regs], LTLA_name = regions[[nm]][regs], specimen_date = fulldates)
    fakeup(caselisting)
}

fakeup <- function(caselisting){
    caselisting$FINALID = "fakeID"
    caselisting$pillar = "fakepillar"
    caselisting$NHSER_code = "fake NHSER_code"
    caselisting$NHSER_name = "fake NHSER_name"
    caselisting$PHEC_code = "PHEC code"
    caselisting$PHEC_name = "PHEC name"
    caselisting$UTLA_code = "UTLA code"
    caselisting$UTLA_name = "UTLA name"
    ## LTLA_code ** Used
    ## LTLA_name
    caselisting$sex = "mfx"
    caselisting$age = 50
    caselisting$Onsetdate = as.Date("1970-01-01")
    ## specimen_date ** Used
    caselisting$lab_report_date = as.Date("2000-01-01")
   caselisting[,c("FINALID", "pillar", "NHSER_code", "NHSER_name", "PHEC_code", 
                   "PHEC_name", "UTLA_code", "UTLA_name", "LTLA_code", "LTLA_name", 
                  "sex", "age", "Onsetdate", "specimen_date", "lab_report_date")] 
}

write_linelisting <- function(caselisting, xlsout){
    write.xlsx(caselisting, xlsout)
}

make_region <- function(nregions,
                        xmin=87357,
                        ymin=7440,
                        xmax = 655617,
                        ymax = 657536){
    
    pts = st_multipoint(cbind(runif(nregions,xmin,xmax), runif(nregions,ymin,ymax)))
    b = st_sfc(st_buffer(pts, dist=75000), crs=27700)
    v = st_voronoi(pts)
    d = data.frame(lad19nm=paste0("NameF",1:nregions)) # F is for Fake
    d$lad19cd = sprintf("F%05d",1:nregions)
    d$geom=st_collection_extract(v)
    d = st_as_sf(d, crs=27700)
    d = st_intersection(d, b)
    d$popsize = round(runif(nrow(d),22000,1000000)) # rough range of LTLAs
    return(d)

}

write_region <- function(regiondata, dst){
    st_write(regiondata, dst, delete_layer=TRUE)
}


make_flow <- function(regiondata, code="lad19cd", pop="popsize"){
    codes = regiondata[[code]]
    pops = regiondata[[pop]]
    ## take 0.01 of the product of the populations as the flow
    pp = expand.grid(sqrt(pops), sqrt(pops))
    pprod = round(apply(pp, 1, prod) * 0.01)
    ft = expand.grid(codes, codes)
    data.table(Flow=pprod, From=ft[,1], To=ft[,2])
}

write_flow <- function(flow, rdsfile){
    saveRDS(flow, rdsfile)
}


make_sample_data <- function(outdir, nregions=150, dailytotal=2000){
    regions = make_region(nregions)
    make_sample_data_from_regions(outdir, regions)
}

make_sample_data_from_regions <- function(outdir, regions){
    flows = make_flow(regions)
    traffic = make_traffic("2020-02-01","2020-06-15")
    cases =  make_epi(regions, "2020-02-01", "2020-08-01")
    ## dailycounts = make_poisson_case_counts("2020-02-01", "2020-08-01", dailytotal)
    ## cases = make_linelisting(regions, dailycounts)
    write_all(regions, flows, traffic, cases, outdir)
}

write_all <- function(regions, flows, traffic, cases, outdir){
    write_region(regions, file.path(outdir, "regions.gpkg"))
    write_flow(flows, file.path(outdir, "flows.rds"))
    write_traffic(traffic, file.path(outdir, "traffic.xlsx"))
    write_linelisting(cases, file.path(outdir, "linelisting.xlsx"))
    
}


make_epi <- function(regions, startDate, endDate, startRate=.00001, endRate=.0005){
    dates = seq(as.Date(startDate), as.Date(endDate),1)
    nd = length(dates)
    nr = nrow(regions)
    cc = rbinom(n=nd*nr, size=regions$popsize, prob=rep(seq(startRate, endRate, len=nd), rep(nr, nd)))
    cc = matrix(cc, nr, nd, byrow=FALSE)
    totalregion = apply(cc,1, sum)
    cases = data.frame(regionI = rep(1:nr, totalregion))
    cases$specimen_date = rep(rep(dates,nr), as.numeric(t(cc)))
    cases$LTLA_code = regions$lad19cd[cases$regionI]
    cases$LTLA_name = regions$lad19nm[cases$regionI]
    cases$regionI <- NULL
    fakeup(cases)
}

make_exp_epi <- function(regions, startDate, endDate, startRate, decayRate, finalRate){
    dates = seq(as.Date(startDate), as.Date(endDate),1)
    rate = finalRate + startRate * exp(-(0:(length(dates)-1))/decayRate)
    st = expand.grid(regions$popsize, rate)
    nc = rbinom(nrow(st), size=st[,1], prob=st[,2])
    st = expand.grid(region = 1:nrow(regions), specimen_date = dates)
    st$count = nc
    data.table::setDT(st)
    cases = st[rep(1:nrow(st),st$count),]
    cases$count=NULL
    cases$LTLA_code = regions$lad19cd[cases$region]
    cases$LTLA_name = regions$lad19nm[cases$region]
    cases$region = NULL
    fakeup(cases)
}

create_data_for_region <- function(regions, outdir, writeregions=FALSE, boost=NA){
    flows = make_flow(regions)
    traffic = make_traffic("2020-02-01","2020-06-15")
    cases =  make_exp_epi(regions, "2020-02-01", "2020-08-01",startRate=0.00002, 70, .000004)
    if(!all(is.na(boost))){
        cases =  boost_random_regions(
            cases,
            boost$nregions,
            seq(max(cases$specimen_date),by=-1,length=boost$recent) ,boost$factor)
    }
    message("flows...")
    write_flow(flows, file.path(outdir, "flows.rds"))
    message("traffic...")
    write_traffic(traffic, file.path(outdir, "traffic.xlsx"))
    message("cases...")
    write_linelisting(cases, file.path(outdir, "linelisting.xlsx"))
    message("total cases: ",nrow(cases))
    if(writeregions){
        message("regions...")
        write_region(regions, file.path(outdir, "regions.gpkg"))  
    }else{
        message("not writing regions...")
    }
    
}

boost_random_regions <- function(cases, nregions, boostDates, mfactor){
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    boosted_regions = resample(unique(cases$LTLA_code), nregions)
    message("Boosting cases in ",paste0(boosted_regions, collapse=","), " for ", paste0(boostDates,collapse=","))
    boost_region(cases, boosted_regions, boostDates, mfactor)
    
}

boost_region <- function(cases, region_codes, boostDates, mfactor){
    bd = as.Date(boostDates)
    rcases = cases[cases$LTLA_code %in% region_codes & cases$specimen_date %in% bd,]
    nc = nrow(rcases)
    nnew = rpois(1, mfactor*nc)
    cuts = nnew*c(0,sort(runif(nc-1)), 1 )
    neach = round(diff(cuts))
    newcases = rcases[rep(1:nc, neach),]
    rbind(cases, newcases)
}
