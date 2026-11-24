### Set options for DiCE.R

## diceOptions

# Here you can set the location of the temporary directory.
# The directory will be created by default in the DiCE.R main directory.
# The 'tempDir' folder is the cornerstone of DiCE.R bookkeeping of jobs.
# Therefore, when computing with multiple computers
# the 'tempDir' folder has to point to the same location on a shared storage.
# e.g.,
diceOptions$tempDir <- "./temp"

# Set simulation file name. Note that this name is the default so
# defining it is optional.
diceOptions$simulation <- "simulation.R"

# Set factorial design path collapse for these levels of non-duplicated data.
# These paths are not stored as subdirectories but will rather be collapsed to a
# comma-separated list and stored in a file "extraparams". Used only in index-type
# simulations.
# e.g., diceOptions$uniqueParamLevels <- 1:2

# The type of parameters. May be 'multifactorial' or 'index'.
diceOptions$type <- "multifactorial"

## diceParams - Set factorial design parameters.
diceParams <- list()

# The parameter vectors can be named as wanted
# and they can be numbers or character strings.
# At least one parameter is required.
diceParams$type <- c("Cheddar", "Gouda", "Emmental")
diceParams$layer <- 1:10
diceParams$piece <- 1:20

## sharedOptions - Set additional options for running the computations.
# These are not mandatory but are delivered to the simulation script
# and can be anything outside the parameters your simulation script needs.
sharedOptions$resultDir <- "Results"