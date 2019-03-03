
#' Create correlation of specified size
#'
#' @param vector1 Optional: a vector for which a correlated vector should
#'                be created.
#' @param vector2 Optional: a vector that should be reshuffled in order to
#'                create a correlation with vector1.
#' @param r Desired size of the correlation. Only takes values between -1
#'          and 1.
#' @param shuffles Specifies how many times vector2 should be shuffled at
#'                 every step of the function. Defaults to 100.
#' @param N Optional: Size of the two vectors to be created.
#' @param MW1 Optional: mean of the first vector to be drawn from a normal
#'            distribution.
#' @param SD1 Optional: standard deviation of the first vector to be
#'            drawn from a normal distribution.
#' @param MW2 Optional: mean of the second vector to be drawn from a
#'            normal distribution. Takes value of MW1 if not provided.
#' @param SD2 Optional: standard deviation of the second vector to be
#'            drawn from a normal distribution. Takes value of MW1 if not
#'            provided.
#'
#' @details
#'
#' Description yet to come.
#'
#' @examples
#'
#' cor.7 <- sim_cor(N = 50, MW1 = 10, SD1 = 1, MW2 = 4, SD2 = 2, r = .7,
#'                  shuffles = 1000)
#'
#' @author Juli Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
sim_cor <- function(vector1 = NULL, vector2 = NULL, r, shuffles = 100, N = NULL, MW1 = NULL,
                    SD1 = NULL, MW2 = NULL, SD2 = NULL) {
  # a) validate input
  # b) determine function mode (vector or parameter)
  # c) replace all missing arguments for second vector with arguments for first vector
  validation <- val_input_cor(vector1, vector2, r, shuffles, N, MW1, SD1, MW2, SD2)

  # see c): take all arguments from validation, in case replacements have taken place
  function_mode <- validation[[1]]
  vector1 <- validation[[2]]
  vector2 <- validation[[3]]
  N <- validation[[4]]
  MW1 <- validation[[5]]
  SD1 <- validation[[6]]
  N2 <- validation[[7]]
  MW2 <- validation[[8]]
  SD2 <- validation[[9]]

  # generate vectors from parameters
  if(function_mode == "parameter") {
    vector1 <- rnorm(N, MW1, SD1)
    vector2 <- rnorm(N2, MW2, SD2)
  }

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
  MW1 <- mean(vector1)
  SD1 <- sd(vector1)
  N2 <- length(vector2)
  MW2 <- mean(vector2)
  SD2 <- sd(vector2)

  # store the two vectors and information on parameters of both vectors
  output <- list(data = data.frame(x = vector1, y = vector2), N1 = N1, MW1 = MW1, SD1 = SD1,
                 N2 = N2, MW2 = MW2, SD2 = SD2, desired_correlation = r,
                 actual_correlation = correlation, shuffles = shuffles,
                 function_mode = function_mode)

  return(output)
}

# check if input is valid and determine if vector or parameter mode
val_input_cor <- function(vector1, vector2, r, shuffles, N, MW1, SD1, MW2, SD2) {
  ## GENERAL
  N2 <- N

  if(r > 1 | r < -1) {
    stop("r can only take values between -1 and 1")
  }

  validate_input(r, "r", "numeric", 1)

  validate_input(shuffles, "shuffles", "numeric", 1, TRUE, TRUE)

  ## VECTOR MODE
  if(argument_exists(vector1)) {
    # if at least one of N, MW1, SD1, N2, MW2, SD2 is not NULL
    if(!all(sapply(list(N, MW1, SD1, N2, MW2, SD2), is.null))){
      stop("vector1 and vector2 must not be combined with N, MW1, SD1, MW2 or SD2")
    }

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

    function_mode <- "vector"
  }

  if(argument_exists(vector2)) {
    if(is.null(vector1)) {
      stop("vector1 must be provided when vector2 is used")
    }
  }

  ## PARAMETER MODE
  ## GROUP 1
  if(argument_exists(N)) {
    if(sum(sapply(list(MW1, SD1), is.null)) != 0) {
      stop("MW1 and SD1 must be provided if N is used")
    }

    validate_input(N, "N", "numeric", 1, TRUE, TRUE)
    function_mode <- "parameter"

    # DEALING WITH MISSING ARGUMENTS FOR SECOND GROUP
    # If at least one of WM2 and SD2 is not provided - take missing argument from N, MW2
    # and/or SD1, respectively
    # But warn the user
    if(sum(sapply(list(MW2, SD2), is.null) != 0)) {
      replacements <- rep(NA, 2)

      if (is.null(MW2)) {
        MW2 <- MW1
        replacements[2] <- "MW"
      }

      if (is.null(SD2)) {
        SD2 <- SD1
        replacements[3] <- "SD"
      }
      warning(paste0((replacements[!is.na(replacements)]), 2, collapse = " and "),
              " taken from ",
              paste0((replacements[!is.na(replacements)]), 1, collapse = " and "))
    }
  }

  # error when MW1 or SD1 are provided without N
  if(argument_exists(MW1)) {
    if(sum(sapply(list(N, SD1), is.null)) != 0){
      stop("N and SD1 must be provided if MW1 is used")
    }

    validate_input(MW1, "MW1", "numeric", 1)
  }

  if(argument_exists(SD1)) {
    if(sum(sapply(list(N, MW1), is.null)) != 0){
      stop("N and MW1 must be provided if SD1 is used")
    }

    validate_input(SD1, "SD1", "numeric", 1)
  }

  ## GROUP 2
  # if any parameters for second group are provided, N must exist
  if(sum(sapply(list(MW2, SD2), is.null)) == 0) {
    if(is.null(N)){
      stop("N must be provided if group 2 parameters are used")
    }
  }

  if(argument_exists(MW2)) {
    validate_input(MW2, "MW2", "numeric", 1)
  }

  if(argument_exists(SD2)) {
    validate_input(SD2, "SD2", "numeric", 1)
  }

  return(list(function_mode, vector1, vector2, N, MW1, SD1, N2, MW2, SD2))
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
