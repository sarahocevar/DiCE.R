#!/bin/sh

echo "Enter number of Rscripts to open:"

read -r useproc

if ! [[ $useproc =~ $re ]] ; then
   echo "error: Not a number" >&2; exit 1
fi

for i in `seq 1 $useproc` 
do
  Rscript DiCE.R &
  echo $i
  sleep 1
done
