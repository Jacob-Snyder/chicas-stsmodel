#!/bin/bash

OPTIND=1
output=""
unrestricted=""
restricted=""
linelist=""

while getopts "o:u:r:l:" opt; do
    case "$opt" in
	o)
	    output=$OPTARG
	    ;;
	u)
	    unrestricted=$OPTARG
	    ;;
	r)
	    restricted=$OPTARG
	    ;;
	l)
	    linelist=$OPTARG
	    ;;
    esac
	    
done

shift $((OPTIND-1))
[ "${1:-}" = "--" ] && shift

err="0"

if [ "$linelist" == "" ]
then 
   echo "No -l <linelist> option given"
   err=1
fi

if [ "$restricted" = "" ]
then 
   echo "No -r <restricted> option given"
   err=1
fi
if [ "$unrestricted" = "" ]
then 
   echo "No -u <unrestricted> option given"
   err=1
fi
if [ "$output" = "" ]
then 
   echo "No -o <output> option given"
   err=1
fi
if [ "$err" = "1" ]
then
    echo "Missing or bad arguments given"
    exit -1
fi
   
export linelist
export restricted
export unrestricted
export output

echo Start
date

R --no-save <<EOF
linelist = Sys.getenv("linelist")
output = Sys.getenv("output")
restricted = Sys.getenv("restricted")
unrestricted = Sys.getenv("unrestricted")

message("linelist is ",linelist)
message("output is ",output)
message("restricted folder is ",restricted)
message("unrestricted folder is ",unrestricted)

library(devtools)
load_all("./stsmodel")
launch(output, linelist, unrestricted, restricted)

EOF

echo Done
date
