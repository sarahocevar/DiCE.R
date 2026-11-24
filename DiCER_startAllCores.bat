@echo off
for /l %%x in (1, 1, %NUMBER_OF_PROCESSORS%) do ( 
  start Rscript DiCE.R
  if %%x==1 (timeout 1)
)