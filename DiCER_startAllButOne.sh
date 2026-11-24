#!/bin/sh

useproc=$(($(nproc) -1))

for i in `seq 1 $useproc` 
do 
  Rscript DiCE.R &
  echo $i
  sleep 1
done
