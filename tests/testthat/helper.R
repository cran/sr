get_pt <- function(depth) {
  he <- embed(henon_x, depth+1)
  he <- he[1:200, ]
  p <- he[ ,2:(depth+1)]
  t <- he[ ,1]
  return(list(p = p, t = t))
}

scale01 <- function(x) {
  maxx <- max(x)
  minx <- min(x)
  return(scale(x,
               center = minx,
               scale = maxx - minx))
}

plot_series <- function(avector) {
  print(ggplot(data = data.frame(idx=1:length(avector), x=avector)) +
    geom_point(mapping = aes(x = idx, y = x), shape = ".") +
    labs(title = "mw test", x = "time step", y = "value"))
}


get_mw_test <- function() {
  vraw <- var(mgls)
  n1 <- rnorm(2640,
             mean = mean( mgls ),
             sd = sqrt( vraw * .20 ))
  n2 <- rnorm(2359,
              mean = mean(mgls),
              sd = sqrt( vraw * .40))

  # add noise, check it out
  mglsn <- mgls + append(n1, n2)
  mglsn <- scale01(mglsn)
  mglse <- embed(mglsn, 17)
  p <- mglse[ , 2:17]
  t <- mglse[ , 1]
  return(list(p = p, t = t))
}


