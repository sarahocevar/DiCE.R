@echo off

set tempDir=

set /p tempDir=Enter temporary folder name: 

echo Collecting log info from folder %tempDir%...

if [%tempDir%]==[] (  echo Using temporary directory defined in config.R & Rscript Utils\collect_log.R & timeout 3 & exit /B 
) 

if exist %tempDir% (
	echo Using temporary directory %tempDir% & timeout 3 & Rscript Utils/collect_log.R %tempDir% & exit /B
)	
		
echo Error: temporary directory not found! & timeout 3 & exit /B