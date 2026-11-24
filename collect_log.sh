#!/bin/sh

echo "Enter temp directory name:"

read -r tempDir

if [ -z "$tempDir" ]
then 
   echo "Using temporary directory defined in config.R."
   Rscript Utils/collect_log.R 
   sleep 3
elif [ -d "$tempDir" ] #if  tempDir dir exists
then

	Rscript Utils/collect_log.R $tempDir
else 
  echo "Error: temporary directory not found!"
  sleep 3
fi
