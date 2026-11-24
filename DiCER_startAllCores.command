
cd `dirName $0`
for i in `seq 1 $(getconf _NPROCESSORS_ONLN)` 
do 
  Rscript DiCE.R &
  echo $i
  sleep 1
done
