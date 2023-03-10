#' Mackey-Glass time delayed differential equation
#'
#' 4999 data points
#'
#' @docType data
#'
#' @usage mgls
#'
#' @keywords datasets
#'
#' @references See Wikipedia entry on "Mackey-Glass equations"
#'
#' @examples
#' mgls_embedded <- embed(as.matrix(mgls), 25)
#' targets <- mgls_embedded[ ,1]
#' predictors <- mgls_embedded[ ,2:25]
"mgls"
