################################################################################
#
#		Decentralized Distributed Computing Environment for R (DiCE.R)
#
#		v. 1.0, 16 November 2022
#		doi: xxxxx
#
#		This software is distributed without any warranty.
#
#
################################################################################
#
#		Initialize DiCE.R based on config.R, utils.R and simulation.R scripts
#

t1 <- Sys.time()

# start tryCatch initialize DiCE.R (print errors to error log)
tryCatch({

  source("Utils/utils.R")
  initialize()  #initialize from utils.R
}, error = function(e) {

  cat(paste0("Can not initialize DiCE.R. Error: ", e, "\n"))
  print(e)
  quit()

})


# start tryCatch DiCE.R steps after init (print errors to error log)
tryCatch({

  # Check if the job pile exist or is being created
  dice$firstJob <- TRUE
  if (file.exists(dice$file$pileReady) || file.exists(dice$file$generatingPile)) {
    dice$firstJob <- FALSE
  }

  # Create the job pile if not
  if (dice$firstJob) {
    generatePile()
  } else {
    cat("Initializing")
    for (i in 1:10) {
      # Print status of initialization to the console
      Sys.sleep(1)
      cat(".")
    }
    cat("\n\n")
  }

  ##############################################################################
  #
  #		While jobs remain in the Pile, pick one job from the list
  #		of available jobs in a loop
  #

  while (T) {

    # Get the parameters of one job
    thisJobParams <- pickAJob()

    # If the only parameter is not named, then the pile is empty or an error occurred
    if (is.atomic(thisJobParams) & is.null(names(thisJobParams))) {

      if (thisJobParams == 0) { # All jobs done, exit loop
        break
      } else if (thisJobParams == 1) { # Something went wrong, skip this job
        next
      }

    }

    nextJobLabel <- paste0(thisJobParams, collapse = " ")

    # Read extra parameters from file if index mode is used and diceOptions$uniqueParamLevels
    # does not contain all the variables in the parameter space.

    extraParams <- c()
    if (diceOptions$type == "index") {

      tryCatch({

        extraParams <- read.table(paste0(dice$path$pile, "/",
          paste(thisJobParams, collapse = "/"), "/params"), sep = ",")

      }, error = function(e) {

          next # In case something went wrong

      })

    }

    # Move the job from pile directory to working directory
    status <- tryCatch({

      moveJobFromPileToWorking()

    }, error = function(e) {

      return(1)

    })

    if (status == 1) next # In case something went wrong

    # Source the simulation code

    cat(paste0("Now working on ", nextJobLabel, "\n"))

    dice$simOut <- tryCatch({
      withCallingHandlers(
        source(dice$file$simulation),
        warning = wHandler
      )

      return <- 0       # Return 0 if simulation finished without error, try to move to error folder

    }, error = function(e) {

      status <- moveJobFromWorkingToError(e)  # status is 1 if the move failed, else 0
      if (status == 1) {
        return <- 1       # Return 1 if the move failed (uncontrolled error)
      } else {
        cat(paste0(nextJobLabel, " completed!\n\n"))

        return <- -1    # Return -1 if the move was successful (controlled error)
      }

    })

    # If dice$simOut is -1, then the simulation had a controlled error -> no further operations needed.

    if (dice$simOut == 0) { # If simulation was finished without controlled or uncontrolled error...
      status <- moveJobFromWorkingToCompleted()  # ...then move simulation to completed
      if (status == 1) { # If the WORKING -> COMPLETED move had an error, pick the next job
        # (TODO v.2: MOVE TO FAILED OR SOMETHING IN  v. 2)
        next
      } else { # If the move was successful, print completion message
        cat(paste0(nextJobLabel, " completed!\n"))
        cat("\n\n")
      }

    # Uncontrolled simulation error (moving failed) ->
    # pick the next job (TODO: MOVE TO FAILED OR SOMETHING IN v. 2)

    } else if (dice$simOut == 1) {
      next
    }

  } #end while

}, error = function(e) {

  errMsg <- paste0("Error in DiCE.R ", paste0(thisJobParams, collapse = ","), ",", dice$workerIdrId, ": ", e)
  cat(errMsg)
  cat("Stopping DiCE.R due to an unexpected error\n\n")
  cat(errMsg, file = dice$file$errorLog, append = T) # errlog msg

}, warning = function(e) {

  warMsg <- paste0("Warning in DiCE.R ", paste0(thisJobParams, collapse = ","), ",", dice$workerId, ": ", e)
  cat(warMsg)
  cat("Stopping DiCE.R due to an unexpected warning\n\n")
  cat(warMsg, file = dice$file$errorLog, append = T) # errlog msg

}) # End tryCatch DiCE.R steps after init

cat("Worker finished!\n\nPress [enter] to exit")

invisible(readLines(file("stdin"), 1))