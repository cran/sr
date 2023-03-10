
#' Plot Histogram of Gammas
#'
#' Produces a histogram showing the distribution in a population of Gamma
#' values, used to examine the result of a full embedding search.  Pass the result
#' of fe_search() to this function to look for structure in the predictors.
#' For example, it this histogram is bimodal, there is probably one input variable
#' which is absolutely required for a good predictive function, so the histogram
#' divides into the subset containing that variable, and the others that don't.
#' @export
#' @param fe_results The result of fe_search or full_embedding_search.  A matrix
#' containing a column labeled Gamma, of Numeric Gamma values.
#' It also contains an integer column of masks, but that is not used by this function.
#' @param bins Numeric, number of bins in the histogram
#' @param caption Character string caption for the plot
#' @return a ggplot object, a histogram showing the distribution of Gamma values
#' full embedding search output
#' @examples
#' e6 <- embed(mgls, 7)
#' t <- e6[ ,1]
#' p <- e6[ ,2:7]
#' full_search <- fe_search(predictors = p, target = t)
#' gamma_histogram(full_search, caption = "my data")
#===========================================================
gamma_histogram <- function(fe_results, bins = 100, caption = "")
  #===========================================================
{
  Gamma <- NULL
  ggplot(fe_results, aes(x = Gamma)) +
    geom_histogram(bins=bins, color="#000000", fill="blue") +
    labs(
      title = paste("Distribution of ", nrow(fe_results), " Gammas"),
      caption = caption,
      x = "Gamma values",
      y = "Count"
    )
}

