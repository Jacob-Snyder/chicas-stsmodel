stsmodel
========

This folder is now an R package. A rudimentary one, for sure, but
it can be loaded using `devtools`.

Working from a folder containing this one, you can now do:

```
R> library(devtools)
R> load_all("./stsmodel")
R> area_gpkg ="path/to/UK2019mod_pop.gpkg"
R> linelist = "path/to/Anonymised Combined Line List 20200713.xlsx"
R> trafficdata = "path/to/200616_COVID19_road_traffic_national_table.xlsx"
R> flow = "path/to/mergedflows.rds"
R> alldata = read_all(cases=linelist, flows=flow, traffic=trafficdata, areas=area_gpkg)
[this reads in all the XLSX, CSV etc]
R> launch(outputdir, alldata)
```

where the parameters to `launch` are the folder for the output, and this `alldata` list with all 
the inputs read into it.



Docker
------

The image build process is

```
docker build -t stsmodel .
``` 

This creates an image with a version of R with all the needed packages installed.
It does not include the package itself. To run the code in the package in
the docker image you can mount the `stsmodel` folder into the container
and run it from R.


System Architecture
-------------------

The overall system architecture will be something like:

 * `stsmodel` is scheduled to run regularly on `fhm-chicas-gpu0` using the docker image.
 * it writes outputs to a directory in another repository, `stsweb`, say.
 * these outputs can be written to a directory according to the current date, hence keeping
   historical output runs. 
 * `stsweb` is committed and pushed to `gitlab.com`, and this triggers a CI process that
   builds a public Gitlab Pages site.
   
   
