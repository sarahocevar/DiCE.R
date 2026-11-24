################################################################################
#
#		clear_temp.R
#
################################################################################
#
# A helper script used to clear the temporary directory. It renames the
# temporary directory so that the original temporary directory is free for the
# next simulation run. The renamed directory receives a suffix _old_[datetime].
# The renamed directory may then be deleted manually if not needed anymore.
# clear_temp.R is used because deleting the temporary directory manually may
# take a long time if the number of jobs is very high. This way a new simulation
# may be started immediately without having to wait for the files in the
# temporary directory to be deleted.

rm(list = ls())

workingDirectory <- getwd()

cat("working directory: ", workingDirectory, "\n")

diceOptions <- list()
sharedOptions <- list()

source(paste0(c(workingDirectory, "config.R"), collapse = "/"))

# Preserve the old temporary directory
dirName <- diceOptions$tempDir
if (is.null(dirName)) {
  dirName <- "temp"
}
cat("dirName: ", dirName, "\n")
if (dir.exists(dirName)) {
  timeStamp <- format(Sys.time(), "%y-%m-%d_%H-%M-%S")
  file.rename(dirName, paste0(dirName, "_old_", timeStamp, sep = ""))
}

Sys.sleep(1)

# Create a new empty temporary directory
dir.create(dirName)
