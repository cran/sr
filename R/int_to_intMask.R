# ============================================================
#' Integer to Vector Bitmask
#'
#' Converts the bit representation of an integer into a vector of integers
#'
#' Converts an integer to a vector of ones and zeroes.  Used as a helper
#' function for full_embedding_search, it allows more compact storage of bit masks.
#' The result reads left to right, so the one bit will have index of one in the
#' vector corresponding to lag 1 in an embedding.  Works for masks up
#' to 32 bits
#' @export
#' @param i A 32 bit integer
#' @param length Integer length of the bitmask to produce, must be <= 32
#' @return A vector of integer containing 1 or 0
#' @examples
#' he <- embed(henon_x, 17)
#' t <- he[ , 1]
#' p <- he[ ,2:17]
#' mask <- int_to_intMask(7, 16)     # pick out the first three columns
#' pn <- select_by_mask(p, mask)
#' gamma_test(predictors = pn, target = t)
int_to_intMask <- function(i, length)
{
  if (length > 32)     stop("mask length can't be gt 32")
  return(as.integer(intToBits(i)[1:length]))     # intToBits produces a vector of raw
}
