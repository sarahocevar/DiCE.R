
cd `dirname $0`

Processors=$(getconf _NPROCESSORS_ONLN)

n=1
((Processors_n=n))

for i in `seq 1 $Processors_n` 

do 
  Rscript DiCE.R &
  echo $i
  sleep 1
done
