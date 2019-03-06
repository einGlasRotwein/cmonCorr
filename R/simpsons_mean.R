
#' Create a Simpson's Paradox (based on correlated subgroup means)
#'
#' @param r_mean Initial Pearson correlation of group medians. Not the
#'               same as the overall correlation across subgroups (see
#'               details). If you want a specified overall correlation,
#'               use \code{\link{simpsons_paradox}}.
#' @param r_sub Pearson correlation within the subgroups.
#' @param ngroups Number of subgroups present in the data. Must not be
#'                provided when a vector of subgroup means is used
#'                instead (see \code{means_subgroups} and details).
#' @param nsubgroups Number of cases in each subgroup. Defaults to 50.
#' @param means_subgroups A vector providing the x-mean for each subgroup.
#'                        Must not be used when \code{ngroups} is used
#'                        instead (see details).
#' @param sd_subgroups Standard deviation for the x value of each subgroup.
#'                     Can be either a vector containing standard
#'                     deviations for each subgroup or an integer. In the
#'                     latter case, each subgroup will have the standard
#'                     deviation specified by the integer. Defaults to
#'                     sd = 1 for each group.
#' @param means_subgroups_y An optional vector providing the y-mean for each
#'                          subgroup. Can only be used when
#'                          \code{means_subgroups} is provided. Per default,
#'                          same means as in \code{means_subgroups} are used.
#'                          Note that the order of y-means will be shuffled
#'                          in order to create a correlation.
#' @param sd_subgroups_y Standard deviation for the y value of each subgroup.
#'                       Can be either a vector containing standard
#'                       deviations for each subgroup or an integer. In the
#'                       latter case, each subgroup will have the standard
#'                       deviation specified by the integer. Per default
#'                       identical to \code{sd_subgroups}.
#'
#' @details
#'
#' Creates a Simpson's Paradox by creating a correlation between the means
#' of two variables x and y of a number of subgroups. For this step, the
#' function \code{\link{sim_cor_vec}} is used.
#'
#' Then creates a correlation within subgroups using
#' \code{\link{sim_cor_param}} based on the means created in the first step.
#' If the two correlations specified for each step (\code{r_mean} and
#' \code{r_sub}, respectively) have opposite directions, a Simpson's Paradox
#' is created.
#'
#' One of either \code{ngroups} or \code{mean_subgroups} needs to be provided.
#' That is, if the number of subgroups is specified via \code{ngroups}, the
#' x-means for each group will be \code{1:ngroups}. If the x-means for each
#' subgroup are specified via the vector \code{means_subgroups}, the number
#' of subgroups equals the number of means provided in the
#' \code{means_subgroups}. You can specify the y-means of each subgroup via
#' \code{means_subgroups_y}, but since the y-means will be shuffled in oder
#' to create a correlation across the whole data, this rather sets a range
#' of values for the y-means instead of specific y-means for each group.
#'
#' Based on linear correlations (pearson).
#'
#' Returns a \code{data.frame} that holds the x and y coordinates of the
#' data, a \code{group} column containing the subgroup each case belongs
#' to and the correlation within each subgroup.
#'
#' Note that the overall correlation of the data is not identical with the
#' correlation between subgroup means as specified via \code{r_mean}. In
#' fact, the overall correlation between x and y may substantially differ
#' from that of the subgroup means. Nevertheless, with different values for
#' \code{r_mean} and \code{r_sub} provided, the result should always be data
#' where the overall correlation differs from the one within subgroups. Toy
#' around with it and see which parameters yield the 'best' Simpson's Paradox.
#'
#' @examples
#'
#' simpson <- simpsons_mean(r_mean = .9, r_sub = -.6, ngroups = 4, nsubgroups = 30)
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'

simpsons_mean <- function(r_mean, r_sub, ngroups = NULL, nsubgroups = 50,
                             means_subgroups = NULL, sd_subgroups = 1,
                             means_subgroups_y = NULL,
                             sd_subgroups_y = NULL) {
  # validate input and update variables that have been replaced in the process
  validation <- validate_sim_mean(r_mean, r_sub, ngroups, nsubgroups, means_subgroups,
                                 sd_subgroups, means_subgroups_y, sd_subgroups_y)

  r_mean <- validation$r_mean
  r_sub <- validation$r_sub
  ngroups <- validation$ngroups
  nsubgroups <- validation$nsubgroups
  means_subgroups <- validation$means_subgroups
  sd_subgroups <- validation$sd_subgroups
  means_subgroups_y <- validation$means_subgroups_y
  sd_subgroups_y <- validation$sd_subgroups_y

  # correlate subgroup means
  means_cor <-
    sim_cor_vec(vector1 = means_subgroups, r = r_mean, vector2 = means_subgroups_y)$data

  # correlate subgroups within each other
  subgroups <- cor_subgroups(ngroups, nsubgroups, r = r_sub, means_x = means_cor$x,
                             means_y = means_cor$y, sd_x = sd_subgroups,
                             sd_y = sd_subgroups_y)

  # bind all subgroups in a data.frame
  simpar <- Reduce(function(...) rbind(...), subgroups)
  simpar$group <- factor(simpar$group)

  return(simpar)
}

validate_sim_mean <- function(r_mean, r_sub, ngroups, nsubgroups, means_subgroups, sd_subgroups,
                 means_subgroups_y, sd_subgroups_y){
  validate_input(r_mean, "r_mean", "numeric", 1)
  validate_input(r_sub, "r_sub", "numeric", 1)
  validate_input(nsubgroups, "nsubgroups", "numeric", 1, TRUE, TRUE)

  if(r_mean > 1 | r_mean < -1) {
    stop("r_mean can only take values between -1 and 1")
  }

  if(r_sub > 1 | r_sub < -1) {
    stop("r_sub can only take values between -1 and 1")
  }

  if(argument_exists(means_subgroups_y)) {
    if(is.null(means_subgroups)) {
      stop("means_subgroups_y can only be used if means_subgroups is provided")
    }

    validate_input(means_subgroups_y, "means_subgroups_y", "numeric")

    if(length(means_subgroups_y) != length(means_subgroups)) {
      stop("means_subgroups_y and means_subgroups must be of the same length")
    }
  }

  if(argument_exists(ngroups) && argument_exists(means_subgroups)){
    stop("ngroups and means_subgroups must not be used together")
  }

  if(argument_exists(ngroups)) {
    validate_input(ngroups, "ngroups", "numeric", 1, TRUE, TRUE)
    means_subgroups <- 1:ngroups
  }

  if(argument_exists(means_subgroups)) {
    validate_input(means_subgroups, "means_subgroups", "numeric")
    ngroups <- length(means_subgroups)
  }

  validate_input(sd_subgroups, "sd_subgroups", "numeric")

  if(length(sd_subgroups) == 1) {
    sd_subgroups <- rep(sd_subgroups, ngroups)
  } else if (length(sd_subgroups) > 1){
    if (length(sd_subgroups) != ngroups) {
      stop("sd_subgroups must have the same length as means_subgroups or as specified in ngroups")
    }
  } else {
    stop("sd_subgroups must be at least of length 1")
  }

  if(is.null(means_subgroups_y)) {
    means_subgroups_y <- means_subgroups
  }

  if(argument_exists(sd_subgroups_y)) {
    validate_input(sd_subgroups_y, "sd_subgroups_y", "numeric")
    if(length(sd_subgroups_y) == 1) {
      sd_subgroups_y <- rep(sd_subgroups_y, ngroups)
    }

    if(length(sd_subgroups_y) != length(sd_subgroups)) {
      stop("sd_subgroups_y and sd_subgroups must be of the same length")
    }
  } else {
    sd_subgroups_y <- sd_subgroups
  }

  validation <- list(r_mean = r_mean, r_sub = r_sub, ngroups = ngroups, nsubgroups = nsubgroups,
                     means_subgroups = means_subgroups, sd_subgroups = sd_subgroups,
                     means_subgroups_y = means_subgroups_y, sd_subgroups_y = sd_subgroups_y)

  return(validation)
}
