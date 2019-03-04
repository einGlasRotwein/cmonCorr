
#' Create correlation of specified size (parameter input)
#'
#' @param N Size of the two vectors to be created.
#' @param dist A character string determining the distribution the first
#'             vector should be sampled from. Can be set to 'normal' or 'unif'. See
#'             details.
#' @param arglist A list of arguments provided to the distribution function
#'                that is used to create the first vector. See details.
#' @param r Desired size of the correlation. Only takes values between -1
#'          and 1.
#' @param shuffles Specifies how many times vector2 should be shuffled at
#'                 every step of the function. Defaults to 1000.
#' @param dist2 A character string determining the distribution the second
#'              vector should be sampled from. Can be set to 'normal' or 'unif'.
#'              See details. If not provided, the same distribution as for the first
#'              vector is used.
#' @param arglist2 Optional: a list of arguments provided to the
#'                 distribution function that is used to create the second
#'                 vector. See details. If not provided, the same parameters
#'                 as for the first vector are used.
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
#' Vectors of a desired size are drawn from a specified distribution.
#' Parameters for the distribution like mean, standard deviation or min/max
#' are provided as arguments in a list. Experimental danger zone, as there
#' will not always be error messages when parameters in arglist are not provided
#' correctly. When in doubt, generate vectors to be correlated outside the function
#' and then use \code{\link{sim_cor_vec}}.
#'
#' Distributions to choose from
#'
#' - standard normal distribution as implemented in \code{rnorm} (\code{dist =
#' 'normal'}). See \code{\link{rnorm}} for possible arguments to be passed
#' in \code{arglist}.
#'
#' - uniform distribution as implemented in \code{runif} (\code{dist =
#' 'unif'}). See \code{\link{runif}} for possible arguments to be passed
#' in \code{arglist}.
#'
#' @examples
#'
#' cor.52 <- sim_cor_param(100, "normal", list(mean = 10, sd = 1),
#'                         r = .52, shuffles = 1000)
#'
#' cor.8 <- sim_cor_param(100, "normal", list(mean = 10, sd = 1),
#'                         r = -.8, shuffles = 100,
#'                         "unif", list(min = 1, max = 2))
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
sim_cor_param <- function(N, dist, arglist, r, shuffles = 1000, dist2 = NULL,
                          arglist2 = NULL) {
  # validate input and duplicate parameters for vector1 as parameters for vector2,
  # if parameters for vector2 were not provided
  validation <- val_input_cor_param(N, dist, arglist, r, shuffles, dist2, arglist2)

  # take all arguments from validation, in case replacements have taken place
  dist2 <- validation[[1]]
  arglist2 <- validation[[2]]

  # generate vectors from parameters
  vector1 <- draw_from(N, dist, arglist)
  vector2 <- draw_from(N, dist2, arglist2)

  ## SHUFFLE
  vector2 <- shuffle_in_steps(vector1, vector2, r, shuffles)

  ## PREPARE OUTPUT
  # determine actual correlation to be given back to the user
  correlation <- cor(vector1, vector2)

  # draw parameters from vectors for user information to be stored in output
  # for both, vector mode and parameter mode
  # a) vector mode: informs user about parameters of provided vectors
  # b) parameter mode: determines actual parameters of generated vectors (because parameters
  # might differ slightly from the ones provided as arguments)
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
val_input_cor_param <- function(N, dist, arglist, r, shuffles, dist2, arglist2) {
  validate_input(N, "N", "numeric", 1, TRUE, TRUE)
  validate_input(r, "r", "numeric", 1)
  validate_input(shuffles, "shuffles", "numeric", 1, TRUE, TRUE)
  validate_input(arglist, "arglist", "list")
  validate_input(dist, "dist", len = 1, input_set = c("normal", "unif"))

  if(r > 1 | r < -1) {
    stop("r can only take values between -1 and 1")
  }

  if(argument_exists(dist2)) {
    if(is.null(arglist2)){
      stop("arglist2 must be provided when dist2 is used")
    }

    validate_input(dist2, "dist2", len = 1, input_set = c("normal", "unif"))
  }

  if(argument_exists(arglist2)) {
    if(is.null(dist2)){
      stop("dist2 must be provided when arglist2 is used")
    }

    validate_input(arglist2, "arglist2", "list")
  }

  # here, it is clear that either both or none of dist2 and arglist2 are provided
  # hence, if dist2 does not exist, arglist2 does not, either and both can be
  # safely duplicated from dist and arglist

  if(is.null(dist2)) {
    dist2 <- dist
    arglist2 <- arglist
    warning("dist2 and arglist2 have been taken from dist and arglist")
  }

  return(list(dist2, arglist2))
}
