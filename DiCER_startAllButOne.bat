@echo off
set /a nCores = %NUMBER_OF_PROCESSORS%-1
for /l %%x in (1, 1, %nCores%) do ( 
  start Rscript DiCE.R
    if %%x==1 (timeout 1)
)