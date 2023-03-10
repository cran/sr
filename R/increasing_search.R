#' Increasing Embedding Search engine, used by get/plot increasing_search
#'
#' Adds variables one at a time to the input set, to see how many are needed for prediction.
#'
#' An increasing embedding search is appropriate when the input variables are ordered,
#' most commonly in analyzing time series, when it's useful to know how many previous
#' time steps or lags should be examined to build a model.  Starting with lag 1, the
#' search adds previous values one at a time, and saves the resulting gammas.  These
#' results can be examined using plot_increasing_search()
#' @export
#' @param predictors A vector or matrix whose columns are proposed inputs to a
#' predictive function
#' @param target A vector of double, the output variable that is to be predicted
#' @param plot Logical, set plot = FALSE if you don't want the plot
#' @param caption Character string to identify plot, for example, data being plotted
#' @param show Character string, if it equals "vratio", vratios will be plotted,
#' otherwise Gamma is plotted
#' @return An invisible data frame with three columns, Depth of search, from
#' 1 to ncol(predictors), Gamma calculated using columns 1:Depth as predictors,
#' and vratio corresponding to that Gamma (Gamma / var(target))
#' @examples
#' he <- embed(henon_x, 13)
#' t <- he[ , 1]
#' p <- he[ ,2:13]
#' increasing_search(p, t, caption = "henon data embedded 16")
#' df <- increasing_search(predictors=p, target=t, plot = FALSE)
#===========================================================
increasing_search <- function(predictors,
                              target,
                              plot = TRUE,
                              caption = "",
                              show = "Gamma")
  #===========================================================
{
  if (is.vector(predictors)) {
    stop("increasing embedding on a vector doesn't make sense")
  }
  input_dimension <- ncol(predictors)

  mask <- integer(input_dimension)
  result <- data.frame(Depth = 1:input_dimension,
                       Gamma = double(input_dimension),
                       vratio = double(input_dimension))
  for(i in 1:input_dimension)
  {
    mask[i] <- 1
    inputs <- select_by_mask(predictors, mask)
    g <- gamma_test(predictors = inputs, target = target)
    result$Gamma[i] <- g$Gamma
    result$vratio[i] <- g$vratio
  }

  if (show != "vratio") show <- "Gamma"

  Depth <- NULL    # prevent note for CRAN submission
  if (plot) {
   print(ggplot(data = result) +
         geom_line(mapping = aes(x = Depth,
                                 y = get(show)),
                   color = 'blue') +
         theme(plot.title.position = 'plot',
               plot.title = element_text(hjust = 0.5)) +
         labs(title = "Increasing Embedding",
              caption = caption,
              x = "Embedding depth",
              y = show
              )
        )
  }
  invisible(result)
}
