################################################################################
#
#		collect_log.R
#
################################################################################
#
# A helper script that is used to report the success of DiCE.R and fix possible
# problems. If a path to temporary directory is given as a parameter, the path
# to temporary directory is reset and the multifactorial mode expected.
# Otherwise the parameters are read from the config.R file. It compares the
# bookkeeping files found in the temporary directories to the expected jobs
# based on parameters in the config.R file and reports for missing results. It
# also checks for concurrency of the workers, by verifying that each job was
# handles only by a single worker, and reports possible deviations. Other things
# reported are the total number of expected jobs, the number of completed and
# erroneous jobs, the number of missing jobs (pay attention to this!), the time
# when pile construction started, finished and the whole simulation finished.
# The results are written to a file collect_log_[tempdirpath].txt. In the case
# of some jobs found missing, the script suggests re-running those jobs and
# organizes the pile. In case of many jobs missing, the user may start new
# workers using the DiCE.R startup scripts after running collect_log.R script.

diceOptions <- list()
diceParams <- list()
dice <- list()
mTime <- list()

source("Utils/utils.R")

args <- commandArgs(trailingOnly = T)

# If custom temporary path is given, then expect multifactorial mode
# and reset paths to bookkeeping folders
if (suppressWarnings(length(args) > 0 & args[1] != "")) {
  diceOptions$type <- "multifactorial"
  dice$path$temp <- args[1]
  dice$path$pile <- paste0(dice$path$temp, "/pile")
  dice$path$working <- paste0(dice$path$temp, "/working")
  dice$path$completed <- paste0(dice$path$temp, "/completed")
  dice$path$error <- paste0(dice$path$temp, "/error")

  diceOptions$tempDir <- args[1]
}


# This function returns a vector of strings representing the paths of
# the parameters under the completed or error folders in temp.

expectedPaths <- function() {
  if (diceOptions$type == "multifactorial") paramsDF <- expand.grid(diceParams)
  if (diceOptions$type == "index") paramsDF <- data.frame(diceParams[diceOptions$uniqueParamLevels])
  return(apply(paramsDF, 1, function(x) {
    xx <- paste(c(x), collapse = "/", sep = "")
    gsub(" ", "", xx)
  }))
}


# This function returns a list of job-related statistics, including the number of completed,
# error and missing jobs, and the names of worker-problem jobs, and processing times of each job,
# the lengths of workers accessing each job (should always be one), and the list of workers,
# which have worked on the job.

checkPathsExist <- function() {
  allPaths <- expectedPaths()
  numCompleted <- rep(0, length(expectedPaths()))
  numErr <- rep(0, length(expectedPaths()))
  existCount <- 0
  errorCount <- 0
  missingFiles <- list()
  workerProblemFiles <- list()
  processingTimes <- list()
  queueLengths <- list()
  workerList <- list()
  for (i in seq_len(length(allPaths))) {
    file1 <- NA # Working directory of this job
    file2 <- NA # Completed / error directory of this job
    if (file.exists(paste0(dice$path$working, "/", allPaths[i]))) {
      file1 <- paste0(dice$path$working, "/", allPaths[i])
    }

    if (file.exists(paste0(dice$path$completed, "/", allPaths[i]))) {
      file2 <- paste0(dice$path$completed, "/", allPaths[i])
      # Checks how many workers accessed the completed directory of each job.
      completedFiles <- list.files(paste0(dice$path$completed, "/", allPaths[i]), pattern = "^[0-9]+$")
      numCompleted[i] <- numCompleted[i] + length(completedFiles)
      workerList[[i]] <- completedFiles[1]
      existCount <- existCount + 1
    } else if (file.exists(paste0(dice$path$error, "/", allPaths[i]))) {
      file2 <- paste0(dice$path$error, "/", allPaths[i])
      errorCount <- errorCount + 1
      # Checks how many workers accessed the error directory of each job.
      errorFiles <- list.files(paste0(dice$path$error, "/", allPaths[i]), pattern = "^[0-9]+$")
      numErr[i] <- numErr[i] + length(errorFiles)
      workerList[[i]] <- errorFiles[1]
    } else {
      # If result is missing altogether, report as missing.
      missingFiles[[length(missingFiles) + 1]] <- allPaths[i]
    }

    # If file is not missing but wrong number of workers have accessed, report as problematic.
    if ((file.exists(paste0(dice$path$completed, "/", allPaths[i])) ||
      file.exists(paste0(dice$path$error, "/", allPaths[i]))) && (numCompleted[i] + numErr[i]) != 1) {
      workerProblemFiles[[length(workerProblemFiles) + 1]] <- allPaths[i]
      cat(paste("Problem with ", allPaths[i], ": wrong number of", (numCompleted[i] + numErr[i]),
        " workers accessed.\n"))
    }

    # Processing time
    processingTimes[[i]] <- jobProcessingTime(file1, file2)
    if (is.na(processingTimes[[i]])) {
      cat("Warning! File missing in ", allPaths[i], ": (working) ", file1, " or (completed)", file2, "\n")
    }

    # The number of files in the working directory
    queueLengths[[i]] <- queueLength(file1)
  }

  result <- list(existCount, errorCount, unlist(missingFiles), unlist(workerProblemFiles),
    unlist(processingTimes), unlist(queueLengths), unlist(workerList))

  names(result) <- c("completed", "error", "missingFiles", "workerProblemFiles", "processingTimes",
    "queueLengths", "workerList")

  return(result)
}


# This function reports the number of worker flag files and folders in this directory.
# A worker flag file is named using a sequence of numbers.

queueLength <- function(workingDir) {

  if (is.na(workingDir)) {
    return(NA)
  }
  nrow(file.info(list.files(workingDir, pattern = "^[0-9]+$", full.names = T)))

}


# This function compares the time stamps of two files by comparing the time
# difference between the mTime (modification time) of first file and the cTime
# (last status change) of the last file. The result approximates the time spent
# between writing the two files in minutes. In case of ne file missing, NA is
# returned.

jobProcessingTime <- function(firstFile, lastFile) {
  if (is.na(firstFile) || is.na(lastFile)) {
    return(NA)
  }
  firstStat <- file.info(list.files(firstFile, full.names = T))
  lastStat <- file.info(list.files(lastFile, full.names = T))
  difftime(
    with(lastStat, min(as.POSIXct(ctime), na.rm = T)),
    with(firstStat, max(as.POSIXct(mtime), na.rm = T)),
    units = "mins")
}


# This function returns the total expected number of parameter combinations
# (simulations) based on input parameters given.

expectedResultCount <- function() {
  if (diceOptions$type == "multifactorial") {
    paramSpaceSize <- 1
    for (i in seq_len(length(diceParams[diceOptions$uniqueParamLevels]))) {
      paramSpaceSize <- paramSpaceSize * length(diceParams[diceOptions$uniqueParamLevels][[i]])
    }
    return(paramSpaceSize)
  }
  if (diceOptions$type == "index") {
    return(length(diceParams[[1]]))
  }
}

################################################################################
#
#         collect_log process starts

cat("Reading the bookkeeping files from ", gsub(".*/", "", dice$path$temp), ". This may take a while...\n")

checkResult <- checkPathsExist()

################################################################################
#
#					report DiCE.R statistics

outFile <- paste0("collect_log_", gsub(".*/", "", dice$path$temp), ".txt")

cat(paste0("Writing log to outFile: ", outFile, "\n"))

sink(outFile)

pileGeneratingTime <- file.info(dice$file$generatingPile)$ctime
pileReadyTime <- file.info(dice$file$pileReady)$ctime
jobsFinishedTime <- file.info(dice$file$jobsFinished)$ctime

thisJobsDiffTime <- difftime(jobsFinishedTime, pileGeneratingTime, units = "mins")

cat("DiCE.R statistics\n\n")

cat("Input directory: ", dice$path$temp, "\n")
cat(paste("Parameter space: ", paste(names(diceParams[diceOptions$uniqueParamLevels]), collapse = ","), "\n"))
cat(paste0("Expected number of results: ", expectedResultCount(), "\n"))
cat(paste0("Observed number of results: ", sum(checkResult$completed, checkResult$error), "\n"))
cat(paste0("   observed number of successes: ", checkResult$completed, "\n"))
cat(paste0("   observed number of failures: ", checkResult$error, "\n"))

if (length(checkResult$missingFiles) > 0) {
  cat(paste0("Missing jobs: ", paste(checkResult$missingFiles, collapse = ", "), "\n"))
} else {
  cat(paste0("Missing jobs: -\n"))
}

if (length(checkResult$workerProblemFiles) > 0) {
  cat(paste0("Jobs with a worker-queue problem: ", paste(checkResult$workerProblemFiles, collapse = ", "), "\n"))
} else {
  cat(paste0("Jobs with a worker-queue problem: -\n"))
}

cat(paste0("Number of workers: ", length(table(checkResult$workerList)), "\n"))

jobsPerWorker <- data.frame(table(checkResult$workerList))
colnames(jobsPerWorker) <- c("workerid", "jobs")
cat(paste0("Number of jobs per worker:\n"))
print(jobsPerWorker)

cat(paste0("Summary of processing times per job (minutes):\n"))
print(round(summary(checkResult$processingTimes), 2))
cat(paste0("Queue lengths:"))
print(table(checkResult$queueLengths))
cat(paste0("Pile started: ", pileGeneratingTime, "\n"))
cat(paste0("Pile ready: ", pileReadyTime, ".\n"))
cat(paste0("Jobs finished: ", jobsFinishedTime, ".\n"))
cat(paste0("Total time was ", round(thisJobsDiffTime), " minutes. \n"))
sink()
cat(paste0("The log will be found in the file ", outFile, "\n"))

################################################################################
#
#					ask to rerun missing jobs

if ((length(checkResult$missingFiles) + length(checkResult$workerProblemFiles)) > 0) {

  cat("\n")
  response <- ""
  while (!(response %in% c("y", "Y", "n", "N"))) {

    cat("\n\nDo you want to rerun the problematic jobs (y/n)? ")
    response <- invisible(readLines(file("stdin"), 1))

    if (response %in% c("y", "Y")) {
      cat(paste("Removing flag", dice$file$pileReady, " to run more jobs.\n"))
      cat(paste("Removing flag", dice$file$generatingPile, " to run more jobs.\n"))
      reservations <- file.info(list.files(dice$path$temp, recursive = F, pattern = "^[0123456789]+$", full.names = T))
      cat(paste0("removing pile reservations: ", paste(rownames(reservations), collapse = ", "), "\n"))

      unlink(dice$file$pileReady) # Remove flag for pile-ready status
      unlink(dice$file$generatingPile) # Remove flag for generating pile status
      unlink(dice$file$jobsFinished) # Remove flag for jobs finished status
      unlink(rownames(reservations), force = T) # Remove pile constr. reservations

      # Remove jobs that will be re-run from working
      for (f in c(checkResult$missingFiles, checkResult$workerProblemFiles)) {
        cat("removing job ", f, " from working, completed and error.\n")
        unlink(paste0(dice$path$working, "/", f), recursive = T)
        unlink(paste0(dice$path$completed, "/", f), recursive = T)
        unlink(paste0(dice$path$error, "/", f), recursive = T)
      }

      # Add missing jobs to the pile
      jobsDF <- t(data.frame(lapply(c(checkResult$missingFiles, checkResult$workerProblemFiles), function(x) {
        strsplit(x, "/")[[1]]
      })))
      colnames(jobsDF) <- names(diceParams)
      cat("Adding this list back to the pile:\n")
      print(jobsDF)
      generatePile(jobsDF)
      source("DiCE.R")
    }

  }

}