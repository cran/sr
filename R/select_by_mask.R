#' Select by Mask
#'
#' Select columns from a matrix using an integer bitmap
#'
#' Selects columns from a matrix.  A column is included in the output when the
#' corresponding mask value is 1.
#' @export
#' @param data A numeric matrix in tidy form
#' @param intMask An Integer vector whose length equals number of columns in data
#' @return A matrix containing the columns of data for which intMask is 1
#' @examples
#' e12 <- embed(mgls, 13)
#' tn <- e12[ , 1]
#' pn <- e12[ ,2:13]
#' msk <- integer(12)
#' msk[c(1,2,3,4,6,7,9)] <- 1  # select these columns
#' p <- select_by_mask(pn, msk)
#' gamma_test(predictors = p, target = tn)
#'
#' msk <- int_to_intMask(15, 12)     # pick out the first four columns
#' p <- select_by_mask(pn, msk)
#' gamma_test(predictors = p, target = tn)
#===========================================================
select_by_mask <- function(data, intMask) {
  #===========================================================

  if (ncol(data) != length(intMask)){
    stop("Error select_by_mask: Mask length not equal to input column dimension")
  }
  if (sum(intMask) < 1) {
    stop("Error select_by_mask: empty mask")
  }

  indices <- which(intMask == 1)
  newData <- data[ ,indices]

  return(newData)
}

