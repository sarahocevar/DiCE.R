#!/bin/sh

for i in `seq 1 $(nproc)` 
do 
  Rscript DiCE.R &
  echo $i
  sleep 1
done
