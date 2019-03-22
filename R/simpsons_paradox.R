
#' Create a Simpson's Paradox
#'
#' @param r_tot Desired Pearson correlation of the overall data. Will
#'              not always be met, see details.
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
#' @param sd_subgroups_y Standard deviation for the y value of each subgroup.
#'                       Can be either a vector containing standard
#'                       deviations for each subgroup or an integer. In the
#'                       latter case, each subgroup will have the standard
#'                       deviation specified by the integer. Per default
#'                       identical to \code{sd_subgroups}.
#' @param scaling Optional argument determining how much the y-coordinates of
#'                subgroups shold be shifted in order to create the desired
#'                overall correlation. The larger \code{scaling} is, the larger
#'                will be the overall correlation. That is, if scaling is set
#'                to 0, the subgroup coordinates will not be shifted at all.
#'                Note that the presence of subgroups will be easier to detect
#'                if \code{scaling} is large. Still, the actual overall will be
#'                smaller than specified in \code{r_tot}. Defaults to 1.
#' @param ymin Optional argument determining the smalles y-coordinate present
#'             in your data.
#'
#' @details
#'
#' Creates a Simpson's Paradox by creating a correlation within a number of
#' subgroups and then altering the groups' y-coordinates to create a
#' correlation of the overall data. For the first step, the function
#' \code{\link{sim_cor_param}} is used. The second step relies on the
#' function \code{\link{sim_cor_vec}}. If the two correlations specified for
#' each step (\code{r_sub} and \code{r_tot}, respectively) have opposite
#' directions, a Simpson's Paradox is created.
#'
#' One of either \code{ngroups} or \code{mean_subgroups} needs to be provided.
#' That is, if the number of subgroups is specified via \code{ngroups}, the
#' x-means for each group will be \code{1:ngroups}. If the x-means for each
#' subgroup are specified via the vector \code{means_subgroups}, the number
#' of subgroups equals the number of means provided in the
#' \code{means_subgroups}. Note that you cannot specify the y-means of each
#' group since they will be altered in order to create the desired overall
#' correlation. You can, however, specify the smallest y-coordinate present
#' in your data via \code{ymin}. All data points will be shifted along the
#' y-axis, perserving the correlations of the overall data and the subgroups.
#' That way, you have some degree of control over the range of the y-axis.
#'
#' Based on linear correlations (pearson).
#'
#' Returns a \code{data.frame} that holds the x and y coordinates of the
#' data, a \code{group} column containing the subgroup each case belongs
#' to and the correlation within each subgroup.
#'
#' Note that due to the correlations within subgroups, the overall
#' correlation as specified in \code{r_tot} will not always be achieved.
#' You can toy around with the group mean parameters and the scaling to
#' get more satisfying results, but when subgroup correlations and
#' overall correlation differ widely, this will be harder to achieve.
#' In this case, the presence of subgroups will usually also be easier
#' to detect in the visualisation of the data.
#'
#' If you want to target the correlation between subgroup means, use
#' \code{\link{simpsons_mean}}.
#'
#' @examples
#'
#' simpson <- simpsons_paradox(r_tot = -.8, r_sub = .4, ngroups = 5, nsubgroups = 40,
#'                             scaling = 2)
#'
#' simpson2 <- simpsons_paradox(r_tot = .4, r_sub = .-7, ngroups = 4, nsubgroups = 100,
#'                              scaling = 3, ymin = 10)
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'

simpsons_paradox <- function(r_tot, r_sub, ngroups = NULL, nsubgroups = 50,
                             means_subgroups = NULL, sd_subgroups = 1,
                             sd_subgroups_y = NULL, scaling = 1, ymin = NULL) {
  # validate input and update variables that have been replaced in the process
  validation <- validate_sim_par(r_tot, r_sub, ngroups, nsubgroups, means_subgroups,
                                 sd_subgroups, sd_subgroups_y, scaling, ymin)

  r_tot <- validation$r_tot
  r_sub <- validation$r_sub
  ngroups <- validation$ngroups
  nsubgroups <- validation$nsubgroups
  means_subgroups <- validation$means_subgroups
  sd_subgroups <- validation$sd_subgroups
  sd_subgroups_y <- validation$sd_subgroups_y

  # correlate subgroups within each other
  subgroups <- cor_subgroups(ngroups, nsubgroups, r = r_sub, means_x = means_subgroups,
                             means_y = rep(1, length(means_subgroups)), sd_x = sd_subgroups,
                             sd_y = sd_subgroups_y)

  # bind all subgroups in a data.frame
  simpar <- Reduce(function(...) rbind(...), subgroups)
  simpar$group <- factor(simpar$group)

  # determine shifting factor for each group in order to create correlation
  # to achieve that, a correlation for group means is found and each group's
  # y-coordinate is multiplied with corresponding y-coordinate of the vector
  # correlated with group means

  means_cor <- sim_cor_vec(vector1 = means_subgroups, r = r_tot, vector2 = means_subgroups)$data

  for (i in 1:ngroups) {
    simpar$y[simpar$group == i] <- simpar$y[simpar$group == i] + (means_cor$y[i] * scaling)
    simpar$x[simpar$group == i] <- simpar$x[simpar$group == i] + (means_cor$x[i] * scaling)
  }

  if (argument_exists(ymin)) {
    # find current minimum of y-coordinates in simpar
    curr_min <- min(simpar$y)

    # calculate difference between current and desired min
    y_diff <- ymin - curr_min

    # shift all y-coordinate to meet desired ymin
    simpar$y <- simpar$y + y_diff
  }

  return(simpar)
}

validate_sim_par <- function(r_tot, r_sub, ngroups, nsubgroups, means_subgroups, sd_subgroups,
                             sd_subgroups_y, scaling, ymin){
  validate_input(r_tot, "r_tot", "numeric", 1)
  validate_input(r_sub, "r_sub", "numeric", 1)
  validate_input(nsubgroups, "nsubgroups", "numeric", 1, TRUE, TRUE)

  if(r_tot > 1 | r_tot < -1) {
    stop("r_tot can only take values between -1 and 1")
  }

  if(r_sub > 1 | r_sub < -1) {
    stop("r_sub can only take values between -1 and 1")
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

  if(argument_exists(ymin)) {
    validate_input(ymin, "ymin", "numeric", 1)
  }

  if(argument_exists(scaling)) {
    validate_input(scaling, "scaling", "numeric", 1)
  }

  validation <- list(r_tot = r_tot, r_sub = r_sub, ngroups = ngroups, nsubgroups = nsubgroups,
                     means_subgroups = means_subgroups, sd_subgroups = sd_subgroups,
                     sd_subgroups_y = sd_subgroups_y)

  return(validation)
}
