library(devtools)
load_all("./stsmodel")

cases = get_recent_line_listing("/data/covid19/PHE")
traffic = get_recent_transport("/data/covid19/Dft")

alldata = read_all(cases=cases,
 flows="/app/Data/Unrestricted/processed/mergedflows.rds",
 traffic=traffic,
 areas="/app/Data/Unrestricted/original/geodata/UK2019mod_pop.gpkg")

model_and_map(alldata,outputdir="./stsmap/days/", useDate=TRUE)

## change owner to owner of stsmap because docker makes things as root
ch = "chown -R `stat -c %u stsmap`:`stat -c %g stsmap` stsmap/days/"
system(ch)

