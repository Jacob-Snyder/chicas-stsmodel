#!/bin/bash

docker run -v $PWD/:/app/ -v /data:/data --rm stsmodel  Rscript ./stsmodel/inst/scripts/run.R

cd stsmap
git add days
git commit -a -m "new run on `date`"
git push
