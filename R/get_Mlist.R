#' Discover how Gamma varies with sample size
#'
#' Investigates the effect of sample size by calculating Gamma on larger and larger
#' samples.  Gamma will converge on the true noise in the relationship as sampling
#' density on the function increases.  `get_Mlist` produces a showing M values
#' (sample sizes), and the associated Gammas and vratios. It produces a graph by
#' default, and also returns an invisible data.frame.  The successive samples are
#' taken starting at the beginning of the inputs.  There is no option to sort
#' the input data; if you want the data to be randomized, do that before calling
#'  `get_Mlist`.  The graph will become stable when the sample size is large enough.
#'  If the M list does not become stable, there is not enough data for either the
#'  Gamma test or a successful smooth model.
#' @export
#' @param predictors A Numeric vector or matrix whose columns are proposed
#' inputs to a predictive relationship
#' @param target A Numeric vector, the output variable that is to be predicted
#' @param plot A logical, set this to FALSE if you don't want the plot
#' @param caption Character string to be used as caption for the plot
#' @param show Character string, if it equals "vratio", vratios will be plotted,
#' otherwise Gamma is plotted
#' @param from Integer length of the first data sample, as passed to seq
#' @param by Integer increment in lengths of successive windows, passed to seq
#' @param to Integer maximum length of sample to test, passed to seq
#' @return An invisible data frame with three columns: M (a sample size), Gamma
#' and the associated vratio.  This is ordered by increasing M.
#' @examples
#' he <- embed(henon_x, 13)
#' t <- he[ , 1]
#' p <- he[ ,2:13]
#' get_Mlist(p, t, by = 2, caption = "this data")
#===========================================================
get_Mlist <- function(predictors,
                      target,
                      plot = TRUE,
                      caption = "",
                      show = "Gamma",
                      from = 20,
                      to=length(target),
                      by=20)

#===========================================================
#===========================================================
{
  if (is.vector(predictors)) {
    predictors <- as.matrix(predictors)
  }

  sample_lengths <- seq.int(from=from, to=to, by=by)
  nCalculations <- length(sample_lengths)
  result <- data.frame(M = sample_lengths, Gamma = double(nCalculations), vratio = double(nCalculations))

  for(i in 1 : nCalculations)
  {
    new_predictors <- predictors[1:sample_lengths[i], ]
    new_target <- target[1:sample_lengths[i]]
    g <- gamma_test(new_predictors, new_target)
    result$Gamma[i] <- g$Gamma
    result$vratio[i] <- g$vratio
  }

  if (show != "vratio") show <- "Gamma"

  M <- NULL
  if (plot) {
    print(ggplot(data = result) +
      geom_line(mapping = aes(x = M,
                              y = get(show))) +
      labs(title = "M list",
           caption = caption,
           y = show) +
      theme(plot.title.position = 'plot',
            plot.title = element_text(hjust = 0.5)))
  }

  invisible(result)
}
