#' Full Embedding Search
#'
#' Calculates Gamma for all combinations of a set of input predictors
#'
#' Given a set of predictors and a target that is to be predicted, this search
#' will run the gamma test on every combination of the inputs.  It returns the
#' results in order of increasing gamma, so the best combinations of inputs for
#' prediction will be at the beginning of the list.  As this is a fully
#' combinatoric search, it will start to get slow beyond about 16 inputs.  By default,
#' `fe_search` will display a progress bar showing the time to completion.
#'
#' `fe_search()` returns a data.frame with two columns: Gamma, a sorted vector of
#' Gamma values, and mask, an integer column containing the masks representing the inputs
#' used to calculate each Gamma.  To reconstruct the predictor set for a Gamma,
#' use its mask with int_to_intMask and select_by_mask as shown in their examples.
#'
#' @export
#' @param predictors A vector or matrix whose columns are proposed inputs to a
#'   predictive function
#' @param target A vector of double, the output variable that is to be predicted
#' @param prog_bar Logical, set this to FALSE if you don't want progress bar displayed
#' @param n_neighbors Integer number of near neighbors to use in RANN search,
#' passed to gamma_test
#' @param eps The error limit for the approximate near neighbor search.  This
#' will be passed to gamma_test, which will pass it on to the ANN near neighbor search.  Setting
#' this greater than zero can significantly reduce search time for large data sets.
#' @return An invisible data frame with two columns, mask - an integer mask
#' representing a subset of the predictors, and Gamma, the value of Gamma using
#' those predictors.  The rows are sorted from lowest to highest Gamma.  The
#' return value also has an attribute named target_V, the target variance.
#' To get the vratio (estimated fraction of target variance due to noise), divide
#' any of the Gammas by target_v.
#' @examples
#' e6 <- embed(mgls, 7)
#' t <- e6[ ,1]
#' p <- e6[ ,2:7]
#' full_search <- fe_search(predictors = p, target = t)
#' full_search <- dplyr::mutate(full_search,
#'                              vratio = Gamma / attr(full_search, "target_v"))
fe_search <- function(predictors,
                      target,
                      prog_bar = TRUE,
                      n_neighbors = 10,
                      eps = 0.0) {

  if ( is.vector(predictors) ){
    stop("fe_search: full embedding search doesn't make sense for vector")
  }

  n_observations <- length(target)
  if (nrow(predictors) != n_observations)
    stop("fe_search: input rows and output length must be equal")

  dimension <- ncol(predictors)
  n_tests <- 2 ^ dimension - 1     # subtract one because there is no 000...0 mask

  if (prog_bar)
    pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                           total = n_tests,
                           complete = "=",   # Completion bar character
                           incomplete = "-", # Incomplete bar character
                           current = ">",    # Current bar character
                           clear = FALSE,    # If TRUE, clears the bar when finish
                           width = 100)      # Width of the progress bar


  # pre-allocate array for gamma values
  gammas <- double(n_tests)
  # the mask for each gamma value will be its index

  for(i in 1 : n_tests)
  {
    # integer vector masks are produced as needed
    mask <- int_to_intMask(i, dimension)
    inputs <- select_by_mask(predictors, mask)
    gammas[i] <- gamma_test(inputs, target, eps=eps)$Gamma
    if (prog_bar) pb$tick()
  }
  x <- data.frame(mask = 1:length(gammas), Gamma = gammas)
  x <- x[order(x$Gamma), ]
  attr(x, "target_v") <- var(target)

  invisible(x)
}

