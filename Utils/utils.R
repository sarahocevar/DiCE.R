################################################################################
#
#		utils.R
#
################################################################################
#
# A utility function library for DiCE.R. Consists of initialization of DiCE.R as
# well as functions for job bookkeeping.


################################################################################
#
#		Initialize data lists
#

dice <- list()
diceOptions <- list()
sharedOptions <- list()
diceParams <- list()
thisJobParams <- list()
nextJobLabel <- list()


################################################################################
#
#   Set default parameters and override them if found in the configuration file

dice$workerId <- floor(runif(1, 1, 1000000000)) # workerId is a random number between 1 and 1000000000
dice$OS <- Sys.info()["sysname"] # Operating system
dice$sleepTime <- 0.1

# Set DiCE.R defaults
diceOptions$tempDir <- paste0(c(getwd(), "temp"), collapse = "/")
diceOptions$simulation <- "simulation.R"

# Initialize log variables
dice$logMsg <- ""
dice$logFunction <- "" # function to print to log msg (for identifying the source for the msg)
dice$logSuccess <- NA  # did the action success?
dice$logParams <- NA  # job param combination

# Type can be "multifactorial" (multi-factorial design)
# or "index" (parameter combinations from configuration file)
diceOptions$type <- "multifactorial"

source("config.R")  # Override defaults / add new settings

# These parameters are derived based on diceOptions
dice$path$temp <- diceOptions$tempDir
dice$path$pile <- paste0(dice$path$temp, "/pile", collapse = "")
dice$path$completed <- paste0(dice$path$temp, "/completed", collapse = "")
dice$path$error <- paste0(dice$path$temp, "/error", collapse = "")
dice$path$working <- paste0(dice$path$temp, "/working", collapse = "")

dice$file$generatingPile <- paste0(dice$path$temp, "/_generating_pile.tmp", collapse = "")
dice$file$pileReady <- paste0(dice$path$temp, "/_pile_ready.tmp", collapse = "")
dice$file$jobsFinished <- paste0(dice$path$temp, "/_jobs_finished.tmp", collapse = "")
dice$file$simulation <- diceOptions$simulation
dice$file$log <- paste0(dice$path$temp, "/_log_", dice$workerId, ".txt")
dice$file$errorLog <- paste0(dice$path$temp, "/_errorlog_", dice$workerId, ".txt")

dice$jobListAll <- NA
if (is.null(diceOptions$uniqueParamLevels)) {
  diceOptions$uniqueParamLevels <- seq_along(diceParams)
}
op <- options(digits.secs = 3) # For printing milliseconds in time stamps


################################################################################
#
#		Function to initialize the jobs based on config, utils.R and
#		simulation scripts
#

initialize <- function() {

  # Generate temporary directory
  if (!dir.exists(dice$path$temp)) dir.create(dice$path$temp)

  # Do not run DiCE.R with unsupported OS
  if (!dice$OS %in% c("Windows", "Linux", "Darwin")) {
    stop("Operating system is not supported.")
  }

  # Do not run DiCE.R if simulation file not found
  if (!file.exists(dice$file$simulation)) {
    stop("Simulation file not found.")
  }

  # Do not run DiCE.R if the parameters contain forbidden characters .*/
  if (sum(unlist(lapply(diceParams, function(x) {
     length(grep("[^[:alnum:]_-]", x[is.na(suppressWarnings(as.numeric(x)))])) > 0 })))
     > 0) {
    stop("Parameters contain forbidden characters ./*")
  }

  # Do not run DiCE.R without simulation parameters
  if (length(diceParams) == 0) {
    stop("Input parameters must exist!")
  }

  #Do not run DiCE.R without input parameters
  #TODO: custom list support to v. 2
  if ((is.null(names(diceParams)) || !is.list(diceParams))) {
    stop("Input parameters must be a named list!")
  }

  startLogMsg("utils-read options", paste0("set working dir to ", diceOptions$tempDir))
  printLogMsg()

  # Generate the sub directories in temporary directory
  for (path in dice$path) {
    if (!dir.exists(path)) dir.create(path)
  }
  rm(path)

}


################################################################################
#
#		A function to generate the pile in the temporary directory.
#
#		A bookkeeping file "_generatingPile.tmp" is created at the
#		beginning of pile construction. Before pile construction, a
#		race condition between multiple workers starting to
#		construct the pile due to concurrency issues is prevented
#		using a reservation file for pile construction.
#		The jobs are generated according to the experiment type (multifactorial,
#		index and custom list supported). When all jobs have been created,
#		a bookkeeping file "_pile_ready.tmp" is created.

generatePile <- function(customJobDF = NA) {

  if (length(customJobDF) == 1 && !is.na(customJobDF) || length(customJobDF) > 1) {
    diceOptions$type <<- "custom list"
  }

  # Create "_generatingPile.tmp" file
  write("", file = dice$file$generatingPile)

  # Reservation file for making the pile
  write("", file = paste0(dice$path$temp, "/", dice$workerId))

  # Read reservations
  dirList <- ""
  reservationsListed <- F
  while (reservationsListed == F)
    tryCatch({
      Sys.sleep(0.5)
      details <- file.info(list.files(dice$path$temp, recursive = F, pattern = "^[0123456789]+$"))

      # Order based on file modification time, then based on name
      details <- details[order(as.POSIXct(details$mtime), rownames(details)), ]
      dirList <- rownames(details)
      reservationsListed <- T
    }, error = function(e) {
        cat("error:", paste(e), "Waiting for pile reservations...\n")
        # Sometimes file.info can not access all reservation files. In that case, wait.
      Sys.sleep(1)
    })

  # If this is not the first reservation, skip pile construction
  if (dice$workerId != dirList[1]) {
    cat("Another worker has already made a reservation for pile generation, continue to executing jobs.\n")
    dice$firstJob <<- F
    return(1)
  }

  # Create the pile

  # If a custom data frame exists, then make a pile from that.
  if (diceOptions$type == "custom list") {
    dice$jobListAll <<- customJobDF
    cat("Generating a custom pile...\n")
  } else if (diceOptions$type == "index") {
    dice$jobListAll <<- data.frame(diceParams)
    cat("Generating the pile in the index mode...\n")
  } else {
    dice$jobListAll <<- expand.grid(diceParams)  #... else, the experiment type is "multifactorial" (default)
    cat("Generating the pile in the multi-factorial mode...\n")
  }
  if (length(diceOptions$uniqueParamLevels) == 0 || diceOptions$type == "multifactorial") {
     diceOptions$uniqueParamLevels <<- seq_len(ncol(dice$jobListAll))
     # Use all levels for multifactorial or if not defined with index type
  }

  # How many jobs in total
  dice$nJobs <<- nrow(dice$jobListAll)
  cat(paste0("The pile consists of ", dice$nJobs, " jobs.\n", collapse = ""))

  # Initialize the pile
  cat("This may take a while.\n")
  cat("Initializing the pile...\n")

  pPrint <- seq(1, 100, by = 1)
  iPrint <- ceiling(dice$nJobs * pPrint / 100)
  cat("0% done")
  for (i in 1:dice$nJobs) {
    # Print status of initialization to the console
    p <- pPrint[i == iPrint]
    if (length(p) == 1) {
      cat("\b\b\b\b\b\b\b\b\b")
      cat(paste0(p, "% done", collapse = ""))
    } else if (length(p) > 1) {
      cat("\b\b\b\b\b\b\b\b\b")
      cat(paste0(p[length(p)], "% done", collapse = ""))
    }

    # Add each job to the pile
    nextJob <- dice$jobListAll[i, diceOptions$uniqueParamLevels]
    jobParams <- lapply(nextJob, as.character)
    jobID <- paste0(jobParams, collapse = "/")
    dirName <- paste0(dice$path$pile, "/", jobID, collapse = "")

    dir.create(dirName, recursive = T)

    startLogMsg("utils-generatePile", "add to pile")
    appendLogSuccess(file.exists(dirName))
    setLogParams(jobParams) # At this stage, the parameters are internal to this function so have to set manually

    printLogMsg()

    if (diceOptions$type == "index") {
      # Write extra parameters to params file
      write(paste(dice$jobListAll[i, -diceOptions$uniqueParamLevels], collapse = ","),
        file = paste0(dirName, "/params")
      )
    }
  }

  cat("\nPile ready.\n")

  # create "_pile_ready.tmp" file
  write("", file = dice$file$pileReady)

}


################################################################################
#
#		A function to pick a random set of job parameters from the pile.
#		Returns
#			- A list of named job parameters. The parameters are
#			  converted to numeric if possible.
#			- 0, if the pile is empty; also creates a bookkeeping
#			  file "_jobs_finished.tmp" to inform the other workers.
#			- 1, if the job becomes unavailable (has been picked by another
#			  worker) during the picking process.
#

pickAJob <- function() {

  tmpPath <- dice$path$pile
  nextJob <- list()
  listOfDirectories <- list.dirs(tmpPath, recursive = F, full.names = F)

  if (length(listOfDirectories) == 0 && file.exists(dice$file$pileReady)) {
    print("No more jobs in the pile...")
    write("", file = dice$file$jobsFinished)
    return(0) # All jobs have been taken care of
  }
  if (length(listOfDirectories) == 0) {
    print("Pile generation sluggish. Waiting for more jobs to arrive in the pile....")
    return(1) # Try the next job
  }

  # Choose the next job randomly from the pile
  nextJob[[1]] <- sample(listOfDirectories, 1)

  if (length(diceParams[diceOptions$uniqueParamLevels]) > 1) {
    for (i in 2:length(diceParams[diceOptions$uniqueParamLevels])) {
      tmpPath <- paste0(tmpPath, "/", nextJob[i - 1], collapse = "")

      tmpFiles <- list.dirs(tmpPath, recursive = F, full.names = F)

      # If no jobs in this path
      if (length(tmpFiles) == 0) {
        if (numberOfJobsInPile() == 0 && !file.exists(dice$file$pileReady)) {

          cat("Waiting for more jobs...\n")
          Sys.sleep(0.3)
          return(1)
        }
        cat("Maintaining pile (path requiring attention: ", tmpPath, ").\n ")
        fixStatus <- fixPile(nextJob)
        startLogMsg("utils-pickAJob", "maintaining pile")
        appendLogSuccess(fixStatus == 0)
        printLogMsg()

        # If pile was broken because the DiCE.R finished during picking this job,
        # inform other workers using a bookkeeping file and return 0.
        if (fixStatus == 1) {
          if (numberOfJobsInPile() == 0) {
            write("", file = dice$file$jobsFinished)
            return(0)
          }
        }
        return(1)
      }
      nextJob[[i]] <- sample(tmpFiles, 1)

    }
  }

  # Create parameter array for the simulation code
  thisJobParams <- as.vector(nextJob)
  names(thisJobParams) <- names(diceParams[diceOptions$uniqueParamLevels])

  # Convert parameters to numeric, if possible
  numericParams <- suppressWarnings(!is.na(as.numeric(thisJobParams)))
  thisJobParams[numericParams] <- as.numeric(thisJobParams[numericParams])
  startLogMsg("utils-pickAJob", "picked a job")

  setLogParams(thisJobParams) # At this stage, the parameters are internal to this function so have to set manually
  printLogMsg()

  return(thisJobParams)

}


################################################################################
#
#		A function to move a job from pile to working directory.
#		It then creates a reservation file for the job in the working
#		directory. The file name matches the id of this worker
#		id and contains a time stamp string. Then, it checks whether
#		this reservation is successful (the reservation file for this
#		worker has the earliest time stamp among all workers).
#		Returns:
#		- 0 if the job was moved to working successfully and the
#		  reservation was successful.
#		- 1 if file move from pile to working failed or reservation failed

moveJobFromPileToWorking <- function() {

  nextJobDirInPile <- dice$path$pile
  nextJobDirInWorking <- dice$path$working

  # Log message
  startLogMsg("utils-moveJobFromPileToWorking", "work reservation")

  # Generate the paths in the pile and in the working; and sub directories in working.
  for (i in seq_len(length(diceParams[diceOptions$uniqueParamLevels]))) {

    nextJobDirInPile <- paste0(nextJobDirInPile, "/", thisJobParams[i], collapse = "")
    nextJobDirInWorking <- paste0(nextJobDirInWorking, "/", thisJobParams[i], collapse = "")
    if (i < length(diceParams[diceOptions$uniqueParamLevels]) && !dir.exists(nextJobDirInWorking)) {
      tryCatch({
        dir.create(nextJobDirInWorking)
      },
      # Expected warning: '[path] already exists' if another worker just created the sub directory.
      # Otherwise print unexpected warning to error log.
      warning = function(e) {
        if (!grep("already exists", e)) {
        cat(paste0("[moveJobFromPileToWorking] Warning in ",
          paste0(thisJobParams, collapse = ","), ",", dice$workerId, ": ", e),
          file = dice$file$errorLog, append = T) # errorLog message
        }
      })
    }
  }

  # Move from pile to working with file.rename
  tryCatch({
    file.rename(nextJobDirInPile, nextJobDirInWorking)
  },
  error = function(e) {
    # Unexpected error, print errorLog message
    cat(paste0("[moveJobFromPileToWorking] Error in", paste0(thisJobParams, collapse = ","), ",", dice$workerId, ":", e),
      file = dice$file$errorLog, append = T)
    appendLogSuccess(F) # If moving failed, return error
    printLogMsg()
    return(1)
  },
  warning = function(e) {
    # The expected warning is 'The system cannot find the path specified' from file.rename
    # of pile to work (this is handled by skipping the job) - otherwise print the unexpected
    # warning to error log.
    if (!grep("The system cannot find the file specified", e)) {
      cat(paste0("[moveJobFromPileToWorking] Warning in ",
        paste0(thisJobParams, collapse = ","), ",", dice$workerId, ":", e),
        file = dice$file$errorLog, append = T) # log message
    }
    appendLogSuccess(F)   #if moving failed, return error
    printLogMsg()
    return(1)
  })

  # Make a reservation in the working directory
  status2 <- F
  tryCatch({
    status2 <- file.create(paste0(nextJobDirInWorking, "/", dice$workerId))
    mTime <- file.info(paste0(nextJobDirInWorking, "/", dice$workerId))$mtime
    cat(paste(strptime(mTime, "%Y-%m-%d %H:%M:%OS")), file = paste0(nextJobDirInWorking, "/", dice$workerId))
    status2 <- file.exists(paste0(nextJobDirInWorking, "/", dice$workerId))
  },
  # Expected warning of working dir missing when the job was already taken ('No such file or directory').
  # Otherwise print unexpected warning to errorLog
  warning = function(e) {
    if (!grep("No such file or directory", e)) {
      cat(paste0("[moveJobFromPileToWorking] Warning in",
        paste0(thisJobParams, collapse = ","), ",", dice$workerId, ":", e),
        file = dice$file$errorLog, append = T)
    }
  })


  # If reservation fails, return error
  if (status2 == F) {
    appendLogSuccess(F)
    printLogMsg()
    dice$sleepTime <<- sleepTimeIncrease(dice$sleepTime)
    Sys.sleep(dice$sleepTime)

    return(1)
  }


  # Otherwise reservation succeeded. Then...
  dice$sleepTime <<- sleepTimeDecrease(dice$sleepTime)
  appendLogSuccess(T)
  printLogMsg()

  #...wait for all first reservations to finish writing
  Sys.sleep(0.5)

  # Check if this is not the first reservation made by sorting reservations by time, and skip if so
  reservationsListed <- F
  while (reservationsListed == F) {
    tryCatch({
      # Scanned file names must be characters in order for them to work in some older R versions.
      details <- data.frame(file = list.files(nextJobDirInWorking, pattern = "^[0123456789]+$",
        recursive = F, full.names = T), stringsAsFactors = F)
      times <- NA
      while (sum(!is.na(times)) != nrow(details)) {
        times <-  unlist(lapply(details$file, scan, quiet = T, what = "POSIXlt", sep = "\t"))
        Sys.sleep(0.1)
      }
      details$mtime <- times

      # Order based on creation time, then based on name
      details <- details[order(as.POSIXct(details$mtime), details$file), ]
      dirList <- details$file

      if (nrow(details) > 0 & sum(is.na(details$mtime)) == 0) {
        reservationsListed <- T
      }

      startLogMsg("utils-movefrompiletoworking", paste("Read the reservations: ",
        paste(dirList, collapse = ","), " with times:", paste(details$mtime, collapse = ","), collapse = " "))
      appendLogSuccess(T)
      printLogMsg()

    },
    error = function(e) {

      cat("Error:", paste(e), "Waiting for job reservations for job ", paste(thisJobParams, collapse = ","), "...\n")
      # Sometimes file.info can't access all reservation files. In that case, wait and then skip.
      Sys.sleep(1)

    })
  }

  # If this is not the first reservation, skip this job
  startLogMsg("utils-moveJobFromPileToWorking", paste("checking if this reservation ",
    paste0(nextJobDirInWorking, "/", dice$workerId), " is the first:", dirList[1]))
  appendLogSuccess(T)
  printLogMsg()

  if (paste0(nextJobDirInWorking, "/", dice$workerId) != dirList[1]) {
    startLogMsg("utils-moveJobFromPileToWorking",
      paste("multiple reservations -> skipping this (order is: ",
        paste(dirList, collapse = ","), " with times: ",
          paste(details$mtime), collapse = ","))
    appendLogSuccess(T)
    printLogMsg()

    cat("Skip this job reservation\n")
    return(1)
  }

  # Else this is the first job, continue processing
  startLogMsg(
    "utils-moveJobFromPileToWorking",
      paste("first reservation -> selecting this (order is: ",
        paste(dirList, collapse = ","), " with times: ",
          paste(details$mtime), collapse = ",")
  )
  appendLogSuccess(T)
  printLogMsg()

  # Check if parent empty and remove that too.
  removeParentIfEmpty(nextJobDirInPile)

  return(0)

}


################################################################################
#
#		A function to move a job and the reservation file from
#		working to completed.
#		Returns:
#		1 if the move was successful
#		1 if the move failed

moveJobFromWorkingToCompleted <- function() {

  nextJobDirInWorking <- dice$path$working
  nextJobDirInCompleted <- dice$path$completed

  # Log message
  startLogMsg("utils-moveJobFromWorkingToCompleted", "moving job from working to completed")

  # Generate the paths to working and completed; and sub directories in completed.
  for (i in seq_len(length(diceParams[diceOptions$uniqueParamLevels]))) {
    nextJobDirInWorking <- paste0(nextJobDirInWorking, "/", thisJobParams[i], collapse = "")
    nextJobDirInCompleted <- paste0(nextJobDirInCompleted, "/", thisJobParams[i], collapse = "")
    if (i < length(diceParams[diceOptions$uniqueParamLevels]) && !dir.exists(nextJobDirInCompleted)) {
      tryCatch(
        dir.create(nextJobDirInCompleted),
      warning = function(e) {
        # Expected warning: the path has probably been created before. Error msg: '[path] already exists'.
        # Otherwise print warning to error log.
        if (!grep(" already exists", e)) {
          cat(paste0("[moveJobFromWorkingToCompleted] Error in",
            paste0(thisJobParams, collapse = ","), ",", dice$workerId, ":", e),
            file = dice$file$errorLog, append = T) #log msg
        }
      })
    }
  }

  # Move the job with file.rename
  status <- dir.create(nextJobDirInCompleted)
  file.create(paste0(nextJobDirInCompleted, "/", dice$workerId))

  if (status == F) {
    appendLogSuccess(F)
    printLogMsg()
    return(1)
  } else {
    appendLogSuccess(T)
    printLogMsg()
    return(0)
  }

}


################################################################################
#
#		A function to move a job and the reservation file from
#		working to error
#		Returns:
#		1 if the move was successful
#		1 if the move failed

moveJobFromWorkingToError <- function(e) {

  nextJobDirInWorking <- dice$path$working
  nextJobDirInError <- dice$path$error

  # Log message
  startLogMsg("utils-moveJobFromWorkingToError", "moving from job from working to error")

  for (i in seq_len(length(diceParams[diceOptions$uniqueParamLevels]))) {
    nextJobDirInWorking <- paste0(nextJobDirInWorking, "/", thisJobParams[i], collapse = "")
    nextJobDirInError <- paste0(nextJobDirInError, "/", thisJobParams[i], collapse = "")
    if (i < length(diceParams[diceOptions$uniqueParamLevels]) && !dir.exists(nextJobDirInError)) {
      tryCatch({
        dir.create(nextJobDirInError)
      },
      # The expected warning is '[dir] already exists' when the dir was already created by another folder.
      # Otherwise print the warning to error log.
      warning = function(e) {
        if (!grep("already exists", e)) {
          cat(paste0("[moveJobFromWorkingToError] Warning in",
            paste0(thisJobParams, collapse = ","), ",", dice$workerId, ":", e),
            file = dice$file$errorLog, append = T) # Log message
        }
      })
    }
  }

  # Move the job with file.rename
  status <- dir.create(nextJobDirInError)
  file.create(paste0(nextJobDirInError, "/", dice$workerId))

  if (status == F) {
    appendLogSuccess(F)
    printLogMsg()
    return(1)
  } else {
    appendLogSuccess(T)
    printLogMsg()
    writeErrorFile(nextJobDirInError, e)
    return(0)
  }

}


################################################################################
#
#		A helper function to start a log message (which will be later
#		filled with information and printed to the log file of this
#		worker instance.
#		Input parameters:
#		- string thisFun; add the name of the function where the message was
#		printed from.
#		- string thisMsg; the message content (default is "")
#		- boolean thisSuccess; whether the action was successful or not
#		  (default is NA)
#

startLogMsg <- function(thisFun = "", thisMsg = "", thisSuccess = NA) {

  dice$logFunction <<- thisFun
  dice$logMsg <<- thisMsg
  dice$logSuccess <<- thisSuccess
  setLogParams()

}


################################################################################
#
#		A helper function to add the current job parameters to the help
#		message. By default, add the parameters according to the
#		current job parameters (thisJobParams list).
#		Input parameters:
#		- string listOrVectorOfParams; a custom list of job parameters
#		overriding the default (default = NULL)
#

setLogParams <- function(listOrVectorOfParams = NULL) {

  if (exists("thisJobParams") && length(listOrVectorOfParams) == 0) {
    listOrVectorOfParams <- thisJobParams
  }
  dice$logParams <<- paste(unlist(listOrVectorOfParams), collapse = ",")

}


################################################################################
#
#		A helper function to add more content to the current log
#		message.
#		Input parameters:
#		- string msg; the content to add
#

appendLogMsg <- function(msg) {

  dice$logMsg <<- paste0(dice$logMsg, msg)

}


################################################################################
#
#		A helper function to set the success code of the message.
#		Input parameters:
#		- boolean success; whether to report status as "success" or
#		  "failed"
#

appendLogSuccess <- function(success) {

  dice$logSuccess <<- ifelse(success, "success", "failed")

}


################################################################################
#
#		A helper function to printLogMsg the current log to the log
#		file of this worker. Then a new (empty) log message is
#		initialized.

printLogMsg <- function() {

  cat(paste(dice$workerId, paste0("[", dice$logFunction, "]"), Sys.time(),
    dice$logParams, dice$logMsg, dice$logSuccess, sep = "\t"),
    file = dice$file$log, "\n", append = T)
  startLogMsg()

}


################################################################################
#
#		A helper function to counts the number of jobs (number of
#		files with correct depth) in the pile

numberOfJobsInPile <- function() {

  dirsInPile <- list.dirs(dice$path$pile, recursive = T)
  dirDepthInPileRoot <- nchar(gsub("[^/]", "", dice$path$pile))
  dirDepthsInPile <- nchar(gsub("[^/]", "", dirsInPile))
  return(sum(dirDepthsInPile == (length(diceParams) + dirDepthInPileRoot)))

}


################################################################################
#
#		A helper function to increase sleep time (which is used
#		to slow down this worker when the job reservation fails
#		due to a race condition when a job gets multiple reservations).
#		Input params:
#		- time

sleepTimeIncrease <- function(time) {

  if (time < 3) {
    return(time + 0.2)
  }
  return(time)

}


################################################################################
#
#		A helper function to decrease sleep time (which is used when
#		a single researvation was made).
#		Input params:
#		- time

sleepTimeDecrease <- function(time) {

  if (time > 0.3) {
    return(time - 0.2)
  }
  return(time)

}


################################################################################
#
#		Writes the error message raised by simulation to the file
#		"error.txt" in the error bookkeeping directory.
#		Input parameters:
#		- string nextJobDirInError, error directory of this job
#		- string e, the error message.

writeErrorFile <- function(nextJobDirInError, e) {

  nextJobFileInError <- paste0(nextJobDirInError, "/error.txt", collapse = "")
  sink(file = nextJobFileInError)
  cat(paste0(e, "\n", collapse = "")) # Write the error message that was read
  sink()

  cat(paste0(nextJobLabel, " had an error.\n")) # Print this to console
  cat(paste0(e, "\n", collapse = ""))
  cat("\n\n")

}


################################################################################
#
#		Checks a bookkeeping directory for a job and removes the parent
#		directories until the pile root if they are empty (don't contain
#		any jobs).
#		Input parameters:
#		- string nextJobDir, pile directory of this job
#		Returns
#		- 1, error if the pile is not ready yet (parent not removed)

removeParentIfEmpty <- function(nextJobDir) {

  # Return 0 if the pile is already ready
  if (!file.exists(dice$file$pileReady)) {
    return(0)
  }

  # TODO: check here that only alpha-numeric characters are used, v. 2

  parentDir <- nextJobDir
  for (i in 1:(length(diceParams[diceOptions$uniqueParamLevels]) - 1)) {
    splitPath <- as.vector(unlist(strsplit(parentDir, "/")))
    parentDir <- paste0(splitPath[1:(length(splitPath) - 1)], collapse = "/")
    if (length(list.files(parentDir)) == 0) {
      unlink(parentDir, recursive = T)
      # Log message
      startLogMsg("utils-removeParentIfEmpty", "removed empty parent")
      printLogMsg()
    } else {
      break
    }
  }

}


################################################################################
#
#		Checks a bookkeeping directory for a job and removes the parent
#		directories until the pile root if they are empty (contain no jobs).
#		Input parameters:
#		- string params, the Pile directory of this job
#		Returns:
#		- 0, the pile is ready
#		- 1, pile is not safe to fix (contains forbidden characters).

fixPile <- function(params) {  # TODO: merge with removeParentIfEmpty? v. 2

  # Return 0 if the pile is already ready
  if (!file.exists(dice$file$pileReady)) {
    return(0)
  }

  # Go through all directories and remove empty ones unless they are the deepest level.
  # Don't run the function if the parameters contain some of these: .*/:
  safeToFix <- ifelse(length(grep("[^[:alnum:]]", params[is.na(suppressWarnings(as.numeric(params)))])) == 0, T, F)

  if (safeToFix == F) {
    print("Warning: not safe to fix pile (non-alphanumeric characters observed)")
    return(1)
  }

  tmpDir <- dice$path$pile
  for (i in rev(seq_along(params))) {
    tmpDir <- paste0(dice$path$pile, "/", paste(params[1 : i], collapse = "/"))
    tmpDirs <- list.dirs(tmpDir, recursive = F)
    errNum <- 0
    if (length(tmpDirs) == 0 && file.exists(tmpDir)) {
      startLogMsg("utils-fixPile",
        paste0("fix pile: directory:", tmpDir, " contains subdirectories:",
          paste(tmpDirs, collapse = ","), " -> remove empty"))
      errNum <- unlink(tmpDir, recursive = T)
      appendLogSuccess(errNum == 0)
      printLogMsg()
      if (errNum == 1) {
        return(1)
      }
    }
  }
  return(errNum)

}


################################################################################
#
#		A helper function to be used in the simulation file if the user
#   wants to clear objects from the R workspace except for the
#   essential DiCE.R objects.

clearWorkspace <- function() {

  rm(list = ls()[!(ls() %in% c("sharedOptions", "dice",
    "diceOptions", "diceParams"))])

}


################################################################################
#
#		A helper function to be used in the simulation file if the user
#   wants to combine the input parameters of this job to string. By default,
#   the separator is "_".
#
#   The separator can also be specified using mySep parameter. It may
#   only contain alphanumeric character(s) (numbers, letters) or '_' or '-'.
#   In case of attempting to input other kind of separator, the default is used.

thisJobParamsToString <- function(mySep = "_") {

  if (length(grep("[^[:alnum:]_-]", mySep)) > 0) {
    mySep <- "_"
  }
  return(paste(lapply(thisJobParams, paste, collapse = mySep), collapse = mySep))

}


################################################################################
#
#		A handler for warnings occurring in the simulation code. Prints the warning
#   to console and to error log file.

wHandler <- function(w) {
  warMsg <- paste0("Warning in simulation script ", paste0(thisJobParams, collapse = ","), ",", dice$workerId, ": ", w)
  cat(warMsg)
  cat(warMsg, file = dice$file$errorLog, append = T) # errlog msg
  invokeRestart("muffleWarning")
}
