launch <- function(output, alldata){
    write_metadata(alldata, file.path(output,"input_metadata.csv"))
    last_day = max(as.Date(colnames(alldata$cases)[-1],format="%m/%d/%y"))
    run_meta = list(last_day=last_day,
                    start=Sys.time(),
                    end=NA,
                    info = Sys.info()
                    )
    saveRDS(run_meta, file.path(output,"run_meta.rds"))
    pdf(file.path(output, "Rplots.pdf"))
    # close PDF device on exit from function
    on.exit(dev.off())
    
    #THese are done in the other model
    #message("Part 01")
    #analysis01(output, alldata)
    #message("Part 02")
    #analysis02(output, alldata)
    #message("There is no Part 03")
    #message("Part 04")
    analysis04(output)
    run_meta$end = Sys.time()
    run_meta$duration = run_meta$end - run_meta$start
    saveRDS(run_meta, file.path(output,"run_meta.rds")) ## update when finished
}

read_all_files <- function(
                           area_gpkg ="./Data/Unrestricted/original/geodata/UK2019mod_pop.gpkg",
                           linelist = "./Data/Restricted/PHE/PHE_2020-07-13/Anonymised Combined Line List 20200713.xlsx",
                           trafficdata = "./Data/Restricted/Traffic/200616_COVID19_road_traffic_national_table.xlsx",
                           flow = "./Data/Unrestricted/processed/mergedflows.rds"
                           ){
    alldata = read_all(cases=linelist, flows=flow, traffic=trafficdata, areas=area_gpkg)
    
}

