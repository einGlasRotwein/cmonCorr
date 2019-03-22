#' Functions used by the simpsons paradox functions
#'
#' @return NULL
#'
#' @noRd

# correlate subgroups within each other
cor_subgroups <- function(ngroups, nsubgroups, r, means_x, means_y, sd_x, sd_y) {
  # list storing dataframes with subgroups
  subgroups <- list()

  # vector storing actual correlations of subgroups
  sub_cors <- rep(NA, ngroups)

  for(i in 1:ngroups) {
    temp <- sim_cor_param(nsubgroups, "normal", list(mean = means_x[i], sd = sd_x[i]),
                          r, dist2 = "normal",
                          arglist2 = list(mean = means_y[i], sd = sd_y[i]))
    sub_cors[i] <- temp$actual_correlation
    temp <- data.frame(x = temp$data$x, y = temp$data$y, group = i, sub_cor = sub_cors[i])
    subgroups[[i]] <- temp
  }

  return(subgroups)
}
