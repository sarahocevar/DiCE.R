
cd `dirname $0`

Processors=$(getconf _NPROCESSORS_ONLN)
echo $Processors

((Processors_Minus_one=Processors-1))
echo $Processors_Minus_one

for i in `seq 1 $Processors_Minus_one` 

do 
  Rscript DiCE.R &
  echo $i
  sleep 1
done
