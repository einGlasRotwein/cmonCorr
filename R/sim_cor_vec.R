
#' Create correlation of specified size (vector input)
#'
#' @param vector1 Optional: a vector for which a correlated vector should
#'                be created.
#' @param vector2 Optional: a vector that should be reshuffled in order to
#'                create a correlation with vector1. If not provided,
#'                vector1 will we duplicated as vector2.
#' @param r Desired size of the correlation. Only takes values between -1
#'          and 1.
#' @param shuffles Specifies how many times vector2 should be shuffled at
#'                 every step of the function. Defaults to 1000.
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
#' @examples
#'
#' vec1 <- 1:100
#' vec2 <- 101:200
#' cor.7 <- sim_cor(vec1, vec2, r = .7, shuffles = 1000)
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
sim_cor_vec <- function(vector1, vector2 = NULL, r, shuffles = 1000) {
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

  warning(" - Durchlaeufe Runde 1: ", while_limit)

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

  warning(" - Durchlaeufe Runde 2: ", while_limit)

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

  warning(" - Durchlaeufe Runde 3: ", while_limit)

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

  warning(" - Durchlaeufe Runde 4: ", while_limit)

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

  warning(" - Durchlaeufe Runde 5: ", while_limit)

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

  warning(" - Durchlaeufe Runde 6: ", while_limit)

  return(vector2)
}
