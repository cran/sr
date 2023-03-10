#' Henon Map
#'
#' 1000 x data points from the Henon Map
#'
#' @docType data
#'
#' @usage henon_x
#'
#' @keywords datasets
#'
#' @references See Wikipedia entry on "Henon map"
#'
#' @examples
#' henon_embedded <- embed(as.matrix(henon_x), 3)
#' targets <- henon_embedded[ ,1]
#' predictors <- henon_embedded[ ,2:3]
#' gamma_test(predictors, targets)
"henon_x"
