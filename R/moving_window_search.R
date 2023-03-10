#' Moving Window Search
#'
#' Calculate Gamma values for a window moving through the data.
#'
#' This is used for data sets that are ordered on one or more dimension, such as
#' time series or spatial data.  The search slides a window across the data set,
#' calculating gamma for the data at each step.  A change in causal dynamics will
#' appear as a spike in gamma when the causal discontinuity is in the window.
#' @export
#' @param predictors A Numeric vector or matrix whose columns are proposed inputs
#' to a predictive function
#' @param target A Numeric vector, the output variable that is to be predicted
#' @param plot Logical, set this to FALSE if you don't want the plot
#' @param caption Character string, caption for plot
#' @param show Character string, if it equals "vratio", vratios will be plotted,
#' otherwise Gamma is plotted
#' @param window_size Integer width of the window that will move through the data
#' @param by The increment between successive window starts
#' @return An invisible data frame containing starting and ending positions of
#' each window with its associated gamma
#' @examples
#' he <- embed(henon_x, 13)
#' t <- he[ , 1]
#' p <- he[ ,2:13]
#' moving_window_search(p, t, by = 5, caption = "my data")
#===========================================================
moving_window_search <- function(predictors,
                                target,
                                window_size = 40,
                                by = 1,
                                plot = TRUE,
                                caption = "",
                                show = "Gamma")
#===========================================================
{

  if (is.vector(predictors)) {
    predictors <- as.matrix(predictors)
  }

  n_windows <- floor((length(target) - window_size) / by)
  outp <- data.frame(starts=integer(n_windows),
                     ends=integer(n_windows),
                     Gamma=double(n_windows),
                     vratio=double(n_windows))
  outp$starts <- 1 + 0:(n_windows-1) * by
  outp$ends <- outp$starts + window_size

  for (i in 1:n_windows) {
    p_in_window <- predictors[outp$starts[i]:outp$ends[i],  ]
    t_in_window <- target[ outp$starts[i]:outp$ends[i] ]
    gt <- gamma_test(p_in_window, t_in_window)
    outp$Gamma[i] <- gt$Gamma
    outp$vratio[i] <- gt$vratio
  }

  if (show != "vratio") show <- "Gamma"

  starts <- NULL   # to kill the dreaded NOTE
  if(plot) {
    twid <- floor(n_windows / 12)
    ticks <- seq(from = twid, to = n_windows, by = twid) * by
    print(ggplot(data = outp) +
      geom_line(mapping = aes(x = starts,
                              y = get(show))) +
      scale_x_continuous(breaks = ticks) +
      labs(title = "Moving Window Search",
           caption = caption,
           subtitle = paste("Each window contains ", window_size, " data points"),
           x = "Window start",
           y = show
      ))
  }

  invisible(outp)
}


