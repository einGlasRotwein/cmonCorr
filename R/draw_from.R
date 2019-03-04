
#' Sample data from a specified distribution
#'
#' @param N An integer specifying the size of the sample.
#' @param dist A character string determining the distribution the sample
#'             should be taken from. Can be set to 'normal' or 'unif'. See
#'             details.
#' @param arglist A list of arguments provided to the distribution function
#'                that is used to create the first vector. See details.
#'
#' @details
#'
#' Draws a sample of desired size from a specified distribution.
#' Parameters for the distribution like mean, standard deviation or min/max
#' are provided as arguments in a list.
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
#' samp_norm <- draw_from(100, "normal", list = (10, 2))
#'
#' samp_norm <- draw_from(100, "unif", list = (1, 5))
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#'
#' @noRd

draw_from <- function(N, dist, arglist) {
  validate_input(N, "N", "numeric", 1, TRUE, TRUE)
  validate_input(dist, "dist", len = 1, input_set = c("normal", "unif"))
  validate_input(arglist, "arglist", "list")

  if(dist == "normal") {
    samp <- do.call(rnorm, c(N, arglist))
  }

  if(dist == "unif") {
    samp <- do.call(runif, c(N, arglist))
  }

  return(samp)
}
