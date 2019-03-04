
#' Create correlation of specified size (vector input)
#'
#' @param vector1 A vector for which a correlated vector should be created.
#' @param r Desired size of the correlation. Only takes values between -1
#'          and 1.
#' @param shuffles Specifies how many times vector2 should be shuffled at
#'                 every step of the function. Defaults to 1000.
#' @param vector2 Optional: a vector that should be reshuffled in order to
#'                create a correlation with vector1. If not provided,
#'                vector1 will we duplicated as vector2.
#'
#' @details
#'
#' Reshuffles a vector until a correlation of desired size with another vector
#' is created. Does so by taking the shuffled version of the vector that comes
#' closest to the desired correlation, then only reshuffles part of the data,
#' depending on how close to a sufficient solution the result is.
#'
#' Returns the two vectors with the desired correlation (one left untouched),
#' the other reshuffled along with additional information like mean and SD of
#' the two vectors as well as the desired and actual correlation.
#'
#' Based on linear correlations (pearson).
#'
#' @examples
#'
#' vec1 <- 1:100
#' vec2 <- 101:200
#' cor.7 <- sim_cor_vec(vector1 = vec1, vector2 = vec2, r = .7, shuffles = 1000)
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
sim_cor_vec <- function(vector1, r, shuffles = 1000, vector2 = NULL) {
  # validate input and duplicate vector1 as vector2, if vector2 not provided
  vector2 <- val_input_cor(vector1, vector2, r, shuffles)

  ## SHUFFLE
  vector2 <- shuffle_in_steps(vector1, vector2, r, shuffles)

  ## PREPARE OUTPUT
  # determine actual correlation to be given back to the user
  correlation <- cor(vector1, vector2)

  # draw parameters from vectors for user information to be stored in output
  N1 <- length(vector1)
  M1 <- mean(vector1)
  SD1 <- sd(vector1)
  N2 <- length(vector2)
  M2 <- mean(vector2)
  SD2 <- sd(vector2)

  # store the two vectors and information on parameters of both vectors
  output <- list(data = data.frame(x = vector1, y = vector2), N1 = N1, M1 = M1, SD1 = SD1,
                 N2 = N2, M2 = M2, SD2 = SD2, desired_correlation = r,
                 actual_correlation = correlation, shuffles = shuffles)

  return(output)
}

# check if input is valid and determine if vector or parameter mode
val_input_cor <- function(vector1, vector2, r, shuffles) {
  ## VECTORS
  validate_input(vector1, "vector1", "numeric")

  if(argument_exists(vector2)) {
    validate_input(vector2, "vector2", "numeric")

    if(length(vector2) != length(vector1)) {
      stop("vector1 and vector2 must be of the same length")
    }
  } else {
    vector2 <- vector1
    warning("vector2 has been taken from vector1")
  }

  if(argument_exists(vector2)) {
    if(is.null(vector1)) {
      stop("vector1 must be provided when vector2 is used")
    }
  }

  ## R
  if(r > 1 | r < -1) {
    stop("r can only take values between -1 and 1")
  }

  validate_input(r, "r", "numeric", 1)

  ## SHUFFLES
  validate_input(shuffles, "shuffles", "numeric", 1, TRUE, TRUE)

  return(vector2)
}
