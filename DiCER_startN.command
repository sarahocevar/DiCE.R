#!/bin/sh
cd `dirname $0`
echo "Enter number of Rscripts to open:"
read -r useproc

for i in `seq 1 $useproc`
do
  echo $i
  Rscript DiCE.R &
  sleep 1
done