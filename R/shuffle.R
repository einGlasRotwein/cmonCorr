
#' Functions for shuffling vectors provided by the sim_cor functions
#'
#' @param vector1 A vector for which a correlated vector should
#'                be created.
#' @param vector2 A vector that should be reshuffled in order to
#'                create a correlation with vector1.
#' @param r Desired size of the correlation. Only takes values between -1
#'          and 1.
#' @param shuffles Specifies how many times vector2 should be shuffled at
#'                 every step. Defaults to 1000.
#' @param fraction For \code{partial_shuffe}: fraction of the data that
#'                 should be shuffled.
#'
#' @return NULL
#'
#' @noRd

# Takes two vectors and desired correlation as arguments
# Shuffles vector2 completely
# Returns shuffled version of vector2 that comes closest to desired correlation
# Returns difference between correlation of (vector1 and returned vector) and desired correlation
basic_shuffle <- function(vector1, vector2, r, shuffles) {
  # for storing all the shuffles of vector2
  variants_vector2 <- list()

  # shuffle vector2 as many times as indicated in shuffles
  for (i in 1:shuffles) {
    variants_vector2[[i]] <- sample(vector2, length(vector2))
  }

  ## CORRELATE
  # for storing correlations
  cors <- rep(NA, length(variants_vector2))

  # calculate correlation between vector1 and every shuffled variant of vector2
  for (j in 1:length(variants_vector2)) {
    cors[j] <- cor(vector1, variants_vector2[[j]])
  }

  # calculate difference from desired correlation
  cors_diff <- abs(cors - r)

  # find index of minimal absolute difference
  # and make sure there is only one minimum
  index <- cors_diff == min(cors_diff)

  # if more than one minimum: take the first one (i.e. set everything except first one to FALSE)
  if (sum(index) > 1) {
    index[index == TRUE][-1] <- FALSE
  }

  # return vector with correlation that has been identified as closest to desired r
  # return difference to desired correlation
  vec_and_diff <- list(vector2 = unlist(variants_vector2[index]), diff = min(cors_diff))

  return(vec_and_diff)
}

# Takes two vectors, a desired correlation and a specified fraction as arguments
# Shuffles a fraction of vector2
# Returns shuffled version of vector2 that comes closest to desired correlation
# Returns difference between correlation of (vector1 and returned vector) and desired correlation
partial_shuffle <- function(vector1, vector2, r, shuffles, fraction) {
  # for storing all the shuffles of vector2
  variants_vector2 <- list()

  for (i in 1:shuffles) {
    # vector that can be safely manipulated while keeping vector2 intact
    # reset each time
    temp_vec2 <- vector2
    # randomly determine positions of elements to be shuffled
    # only half of them are re-shuffled
    # if fraction is so small that no elements are selected, vector2 is left untouched
    to_shuffle <- sample(length(vector2), length(vector2)*fraction)
    # reshuffle only selected elements
    temp_vec2[to_shuffle] <- sample(temp_vec2[to_shuffle], length(temp_vec2[to_shuffle]))
    variants_vector2[[i]] <- temp_vec2
  }

  ## CORRELATE
  # for storing correlations
  cors <- rep(NA, length(variants_vector2))

  # calculate correlation between vector1 and every shuffled variant of vector2
  for (j in 1:length(variants_vector2)) {
    cors[j] <- cor(vector1, variants_vector2[[j]])
  }

  # calculate difference from desired correlation
  cors_diff <- abs(cors - r)

  # find index of minimal absolute difference
  # and make sure there is only one minimum
  index <- cors_diff == min(cors_diff)

  # if more than one minimum: take the first one (i.e. set everything except first one to FALSE)
  if (sum(index) > 1) {
    index[index == TRUE][-1] <- FALSE
  }

  # return vector with correlation that has been identified as closest to desired r
  # return difference to desired correlation
  vec_and_diff <- list(vector2 = unlist(variants_vector2[index]), diff = min(cors_diff))

  return(vec_and_diff)
}

# use shuffle and partial shuffle to get closer to desired correlation
shuffle_in_steps <- function(vector1, vector2, r, shuffles) {
  # step1: shuffle and pick best result
  shuffle <- basic_shuffle(vector1, vector2, r, shuffles)
  vector2 <- shuffle$vector2
  temp_diff <- shuffle$diff

  # step2:
  # as long as difference from desired correlation bigger than .7:
  # reshuffle data completely (basic_shuffle)
  # do that max. 10 times to prevent endless loop
  while_limit <- 0

  while(temp_diff > .7 && while_limit < 10) {
    shuffle <- basic_shuffle(vector1, vector2, r, shuffles)
    vector2 <- shuffle$vector2
    temp_diff <- shuffle$diff
    while_limit <- while_limit + 1
  }

  # step3:
  # as long as difference from desired correlation bigger than .5:
  # reshuffle part of the data (partial_shuffle)
  # here: fraction = .7 of the data
  # do that max. 10 times to prevent endless loop
  while_limit <- 0

  while(temp_diff > .5 && while_limit < 10) {
    shuffle <- partial_shuffle(vector1, vector2, r, shuffles, .7)
    vector2 <- shuffle$vector2
    temp_diff <- shuffle$diff
    while_limit <- while_limit + 1
  }

  # step4:
  # as long as difference from desired correlation bigger than .4:
  # reshuffle part of the data (partial_shuffle)
  # here: fraction = .5 of the data
  # do that max. 10 times to prevent endless loop
  while_limit <- 0

  while(temp_diff > .4 && while_limit < 10) {
    shuffle <- partial_shuffle(vector1, vector2, r, shuffles, .5)
    vector2 <- shuffle$vector2
    temp_diff <- shuffle$diff
    while_limit <- while_limit + 1
  }

  # step5:
  # as long as difference from desired correlation bigger than .2:
  # reshuffle part of the data (partial_shuffle)
  # here: half (fraction = .25)
  # do that max. 10 times to prevent endless loop
  while_limit <- 0

  while(temp_diff > .2 && while_limit < 10) {
    shuffle <- partial_shuffle(vector1, vector2, r, shuffles, .25)
    vector2 <- shuffle$vector2
    temp_diff <- shuffle$diff
    while_limit <- while_limit + 1
  }

  # step6:
  # as long as difference from desired correlation bigger than .05:
  # reshuffle part of the data (partial_shuffle)
  # here: half (fraction = .25)
  # do that max. 10 times to prevent endless loop
  while_limit <- 0

  while(temp_diff > .05 && while_limit < 10) {
    shuffle <- partial_shuffle(vector1, vector2, r, shuffles, .1)
    vector2 <- shuffle$vector2
    temp_diff <- shuffle$diff
    while_limit <- while_limit + 1
  }

  # step7:
  # as long as difference from desired correlation bigger than .05:
  # reshuffle part of the data (partial_shuffle)
  # here: half (fraction = .25)
  # do that max. 10 times to prevent endless loop
  while_limit <- 0

  while(temp_diff > .02 && while_limit < 10) {
    shuffle <- partial_shuffle(vector1, vector2, r, shuffles, .05)
    vector2 <- shuffle$vector2
    temp_diff <- shuffle$diff
    while_limit <- while_limit + 1
  }

  return(vector2)
}
