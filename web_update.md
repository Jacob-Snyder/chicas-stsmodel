
# Make a new folder

```
mkdir sts
cd sts
```

# Checkout `stsmap` and `stsmodel`

```
git checkout git@gitlab.com:chicas-covid19/stsmap.git
git checkout git@fhm-chicas-code.lancs.ac.uk:covid-19/stsmodel.git
```

# Update or create new daily results in `stsmap`

Every time you want to update or create the output from new daily data, 
you run the model in a docker container that creates a new output folder.
This output folder is in the `stsmap` folder and gets pushed to gitlab
to create new updated gitlab pages.

## Run the model in a docker container

On `gpu0` there is a docker image that has R with all the required packages. Run
it with the new folder mounted and `/data`:

`docker run -v $PWD/:/app/ -v /data:/data -i -t stsmodel`

### Run `R`:

```
library(devtools)
load_all("./stsmodel")
```

Now read the data in - use the latest line listing data that has been mounted under `/data`:

```
alldata = read_all(cases="/data/covid19/PHE/PHE_2020-07-24/Anonymised Combined Line List 20200724.xlsx", 
 flows="/app/Data/Unrestricted/processed/mergedflows.rds",
 traffic="/data/covid19/DfT/2020-06-16/200616_COVID19_road_traffic_national_table.xlsx",
 areas="/app/Data/Unrestricted/original/geodata/UK2019mod_pop.gpkg")
```

Now run the model and map, putting the output in the `stsmap` repo with a date corresponding to the
latest date in the data. Note this is *not* the date of the output file.

```
model_and_map(alldata,outputdir="./stsmap/days/", useDate=TRUE)
### output dir is /app/stsmap/days/2020-07-23
```

Quit R.

### Set owner

Docker creates everything as root. This gets the UID and GID of the
`stsmap` folder and sets the new day to that:

```
chown -R `stat -c %u stsmap`:`stat -c %g stsmap` stsmap/days/2020-07-23/
```

Now exit docker

## Push new `stsmap`

Add the new output day to the `stsmap` git repo and push to gitlab:

```
cd stsmap
git add days/
git commit -a -m "new data `date`"
git push
```

When you want to run with new data, repeat the above, although you 
don't need to clone `stsmap` and `stsmodel` from scratch, you can 
just pull them for updates from the `sts` folder.

