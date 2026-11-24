@echo off
set /p id=Enter number of workers to start:

for /l %%x in (1, 1, %id%) do ( 
  start Rscript DiCE.R
  if %%x==1 (timeout 1)
)