# This tutorial simulation script carries a piece of cheese taking a variable
# length of time.

# Define the function to execute for carrying a piece of cheese
carryOnePiece <- function(params) {

  # Let's start by randomly drawing (x, y)-coordinates from the
  # smallest square containing the unit circle.
  x <- runif(1, -1, 1)
  y <- runif(1, -1, 1)

  # Next, let's randomly draw the time it takes to carry the next
  # piece from an exponential distribution
  timeToFinish <- rexp(1, 1)

  # Print a message stating the type, piece number, layer number,
  # and the time to finish to the console.
  cat(params$type,
    "piece", params$piece,
    "on layer", params$layer,
    "is being transported.",
    "ETA in", timeToFinish, "seconds.\n")

  # Wait for the piece to get carried
  Sys.sleep(timeToFinish)

  # Occasionally, an error occurs while moving the piece. Let's check
  # if the coordinates are inside the unit circle.
  if (x^2 + y^2 > 1)
    stop("Random error occurred")
}

# Call the function with the parameter values from DiCE.R
carryOnePiece(thisJobParams)