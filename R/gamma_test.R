# ===========================================================
# Gamma Test
# ===========================================================
#' Estimate Smoothness in an Input/output Dataset
#'
#' The gamma test measures mean squared error in an input/output data set, relative
#' to an arbitrary, unknown smooth function.  This can usually be interpreted as testing
#' for the existence of a causal relationship, and estimating the expected error of the
#' best smooth model that could be built on that relationship.
#'
#' @export
#' @references \url{https://royalsocietypublishing.org/doi/10.1098/rspa.2002.1010},
#'             \url{https://link.springer.com/article/10.1007/s10287-003-0006-1},
#'             \url{https://smoothregression.com}
#' @param predictors A Numeric vector or matrix whose columns are proposed inputs to a predictive function.
#' @param target A Numeric vector, the output variable that is to be predicted
#' @param n_neighbors An Integer, the number of near neighbors to use in calculating gamma
#' @param eps The error term passed to the approximate near neighbor search.  The default value
#' of zero means that exact near neighbors will be found, but time will be O(M^2), where an
#' approximate search can run in O(M*log(M))
#' @param plot A Logical variable, whether to plot the delta/gamma graph.
#' @param caption A character string which will be the caption for the plot if plot = TRUE
#' @param verbose A Logical variable, whether to return details of the computation
#' @return If verbose == FALSE, a list containing Gamma and the vratio, If verbose == TRUE,
#' that list plus the distances from each point to its near neighbors, the average of squared distances,
#' and the value returned by lm on the delta and gamma averages.  Gamma is Coefficient 1 of lm.
#' @examples
#' he <- embed(henon_x, 3)
#' t <- he[ , 1]
#' p <- he[ ,2:3]
#' gamma_test(predictors = p, target = t)
gamma_test <- function(predictors,
                       target,
                       n_neighbors=10,
                       eps=0.0,
                       plot=FALSE,
                       caption="",
                       verbose=FALSE)
{
  npoints <- length(target)
  if (is.vector(predictors)) {
    v <- length(predictors)
  }
  else {
    v <- nrow(predictors)
  }
  if (v != npoints) {
    stop("gamma_test: predictors not the same length as target")
  }

  # calculate the near neighbor distance tables,

  # first the deltas, distance to near neighbors in input space,
  # ANN returns the 0th nearest neighbor in column 1, we don't want it
  kl <- n_neighbors + 1
  delta_neighbors <- RANN::nn2(predictors, k = kl)
  delta_neighbors$nn.idx <- delta_neighbors$nn.idx[ ,2:kl]
  delta_neighbors$nn.dists <- delta_neighbors$nn.dists[ ,2:kl]

  indexes <- integer(npoints)
  gamma_distances <- matrix(nrow = npoints, ncol = n_neighbors)

  # average of squared distances
  delta_avgs <- double(n_neighbors)
  gamma_avgs <- double(n_neighbors)

  for (i in 1:n_neighbors) {
    # distances in output space, to near neighbors in input space
    indexes <- delta_neighbors$nn.idx[ ,i]
    gamma_distances[ , i] <- abs(target - target[indexes])

    # average of squared errors by near neighbor position
    delta_avgs[i] <- mean(delta_neighbors$nn.dists[ ,i] ^ 2)
    gamma_avgs[i] <- mean(gamma_distances[ ,i] ^ 2) / 2
  }

  # gamma statistic is the intercept of the gamma regression line
  z 	<- lm(gamma_avgs ~ delta_avgs)
  Gamma 	<- z$coefficients[[1]]
  gradient <- z$coefficients[[2]]
  vratio <- Gamma/var(target)
  # mse <- var(z$residuals)
  x <- y <- NULL
  if (plot) {
    print(ggplot(data = data.frame(y = as.vector(gamma_distances), x = as.vector(delta_neighbors$nn.dists))) +
        geom_point(mapping = aes(x = x, y = y), shape = ".") +
        geom_point(data = data.frame(gamma_avgs, delta_avgs),
                   mapping = aes(x = sqrt(delta_avgs), y = sqrt(gamma_avgs)), color = 'red') +
        geom_abline(mapping = aes(intercept = Gamma, slope = gradient), color = 'blue') +
        labs(
           title = "Near Neighbor Distances",
           subtitle = "target distances by predictor distances, for neighbors in predictor space",
           caption = caption,
           x = "Deltas (p space)",
           y = "Gammas (t space)"
        )
    )
  }
  gtlist <- list(Gamma=Gamma, vratio=vratio)
  if (verbose)
    gtlist <- append(gtlist,
                     list(delta.neighbors=delta_neighbors,
                          gamma.distances=gamma_distances,
                          delta.avgs=delta_avgs,
                          gamma.avgs=gamma_avgs,
                          gamma.lm=z)
    )

  return(gtlist)
}
