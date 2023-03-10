## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 4, 
  fig.height = 3
)

## ----setup--------------------------------------------------------------------
library(sr)
library(ggplot2)
library(nnet)
library(magrittr)

## ----henon, fig.width = 5, fig.height=4, fig.align='center'-------------------

  # henon_x <- read.csv("henon_ix.csv")
  str(henon_x)
  hix <- data.frame(idx = 1:length(henon_x), x = henon_x)
  p <- ggplot(data = hix) +
    geom_point(mapping = aes(x = idx, y = x), shape = ".") +
    labs(title = "Henon Map") +
         xlab("time step") +
         ylab("value")
  p


## ----henonline, fig.width = 5, fig.height=4, fig.align='center'---------------

  p + geom_line(mapping = aes(x = idx, y = x), color = "blue")


## ----embed_henon--------------------------------------------------------------
  search_depth <- 15       # number of candidate predictors
  henon_embedded <- embed(as.matrix(henon_x), search_depth+1)  # plus 1 target
  targets <- henon_embedded[ ,1]
  predictors <- henon_embedded[ ,2:(search_depth+1)]
  
  

## ----plot_increasing----------------------------------------------------------
  increasing_search(predictors=predictors, 
                    target=targets,
                    caption = "henon_x, 15 predictors")


## ----get_increasing-----------------------------------------------------------
  tmp <- increasing_search(predictors=predictors, 
                    target=targets,
                    plot = FALSE)
  tmp


## ----gamma_test---------------------------------------------------------------

  scale01 <- function(x) {
    maxx <- max(x)
    minx <- min(x)
    return(scale(x, 
                 center = minx, 
                 scale = maxx - minx))
  }

  henon_scaled <- scale01(henon_x)
  
  search_depth <- 2
  henon_embedded <- embed(as.matrix(henon_scaled), search_depth+1)
  p <- henon_embedded[ ,2:(search_depth+1)]
  t <- henon_embedded[ ,1]
  gamma_test(predictors = p, target = t)

## ----M_test-------------------------------------------------------------------

  get_Mlist(predictors = p, 
            target = t, 
            caption = "henon_x, 2 predictors")


## -----------------------------------------------------------------------------
lt <- length(t)

train_t <- t[1:600]
train_p <- p[1:600, ]

test_t <- t[601:lt]
test_p <- p[601:lt, ]


## ----nnet_model, cache=TRUE, fig.width=8--------------------------------------

set.seed(3)

n_model <- nnet(x = train_p, y = train_t, size = 2, rang = 0.1,
                decay = 5e-4, maxit = 200 )

predicted_t <- predict(n_model, test_p, type = "raw")

test_result <- data.frame(idx = 1:length(test_t), predicted_t[ ,1], test_t)
colnames(test_result) <- c("idx", "predicted", "actual")


ggplot(data = test_result[1:100, ]) +
  geom_line(mapping = aes(x = idx, y = actual), color = "green") +
  geom_line(mapping = aes(x = idx, y = predicted), color = "blue") +
  geom_line(mapping = aes(x = idx, y = actual - predicted), color = "red") +
  labs(y = "predicted over actual",
      title = "Henon Model Test, first 100 points")




## ----look_close---------------------------------------------------------------
# Gamma estimate of noise sse
train_gamma <- gamma_test(predictors = train_p, target = train_t)$Gamma
train_gamma * length(train_t)

# sum of squared error according to model residuals
sum(n_model$residuals ^ 2)

# sum of squared errors on test data
with(test_result, sum((actual - predicted)^2))


## ----nnet_by_gamma, cache=TRUE, fig.width=8-----------------------------------

estimated_sse <- train_gamma * length(train_t)
gamma_model <- nnet(x = train_p, y = train_t, size = 8, rang = 0.1,
                decay = 1e-5, maxit = 2000, abstol =  estimated_sse)

predicted_t <- predict(gamma_model, test_p, type = "raw")

test_result <- data.frame(idx = 1:length(test_t), predicted_t[ ,1], test_t)
colnames(test_result) <- c("idx", "predicted", "actual")

ggplot(data = test_result[1:100, ]) +
  geom_line(mapping = aes(x = idx, y = actual), color = "green") +
  geom_line(mapping = aes(x = idx, y = predicted), color = "blue") +
  geom_line(mapping = aes(x = idx, y = actual - predicted), color = "red") +
  labs(y = "predicted over actual",
      title = "Henon Model Using Gamma")

# Gamma estimate of error -  gamma times number of observations
estimated_sse  

# error according to model residuals
sum(gamma_model$residuals ^ 2)

# sum of squared errors on test data
with(test_result, sum((actual - predicted)^2))




## ----mgls,  fig.width = 8-----------------------------------------------------

str(mgls)

data.frame(idx = 1:length(mgls), x = mgls) %>%
  ggplot(data = .) +
    geom_point(mapping = aes(x = idx, y = x),
               shape = ".") +
    labs(title = "mgls raw data")


## ----embed_mgls---------------------------------------------------------------

me <- embed(as.matrix(mgls), 13)
pre <- me[ ,2:13]
tar <- me[ ,1]


## ----is_mg_raw----------------------------------------------------------------
  increasing_search(predictors=pre, target=tar, caption="mgls, 12 predictors")


## ----gm_mg_raw----------------------------------------------------------------
  get_Mlist(predictors = pre, target = tar, by = 100, caption="mgls, 12 predictors")


## ----mgls_add_noise-----------------------------------------------------------



# target plus noise with variance .04
t1 <- tar + rnorm(length(tar), 
                  mean = mean(tar), 
                  sd = sqrt(.04))

# target plus noise with variance .075
t2 <- tar + rnorm(length(tar), 
                  mean = mean(tar), 
                  sd = sqrt(.075))

# compare these to the variance of the signal
vraw <- var(tar)
vraw
gamma_test(pre, t1)  # noise var=.04 added
gamma_test(pre, t2)  # noise var=.75 added


## ----Mlist_040----------------------------------------------------------------
  get_Mlist(pre, t1, caption="mgls by 12, .04 noise on output")


## ----mlist_075----------------------------------------------------------------
  get_Mlist(pre, t2, caption="mgls by 12, .075 noise on output")


## ----Mlist_040v---------------------------------------------------------------

  get_Mlist(pre, 
            t1, 
            caption="mgls by 12, .04 noise on output",
            show="vratio")


## ----mlist_075v---------------------------------------------------------------

  get_Mlist(pre, 
            t2, 
            caption="mgls by 12, .075 noise on output", 
            show="vratio")


## ----simple_noise-------------------------------------------------------------

x <- mgls

y <- x + rnorm(length(mgls), mean = mean(mgls), sd = sqrt(.04))
x <- x + rnorm(length(mgls), mean = mean(mgls), sd = sqrt(.04))

mean((y - x) ^ 2)


## ----noise_everywhere---------------------------------------------------------
mg <- mgls + rnorm(length(mgls), mean = mean(mgls), sd = sqrt(.04))
mge <- embed(mg, 13)
pn <- mge[ ,2:13]
tn <- mge[ ,1]

## ----is_with_noise------------------------------------------------------------

  increasing_search(pn, tn, caption="mgls by 12, .04 noise on all points")


## ----gM_with_noise------------------------------------------------------------

  get_Mlist(pn, tn, caption="mgls by 12, .040 noise on all points")


## ----followinup---------------------------------------------------------------
gamma_test(pn, tn)

## ----noise_only---------------------------------------------------------------
noise <- rnorm(5000)

e_noise <- embed(noise, 15+1)
n_target <- e_noise[ , 1]
n_predictors <- e_noise[ , 2:(15+1) ]


## ----noise_is-----------------------------------------------------------------
increasing_search(n_predictors, 
                  n_target, 
                  caption = "random variable")


## ----noise_Ml-----------------------------------------------------------------
  get_Mlist(predictors = n_predictors, 
            target = n_target, 
            by = 100, 
            caption = "randomw variable")

## ----pure_noise_plot----------------------------------------------------------
gamma_test(n_predictors, n_target, plot = TRUE, caption = "random variable")

## ----mixed_plot---------------------------------------------------------------
gamma_test(predictors=pre, target=t1, plot = TRUE, caption = "mixed causality and noise")

## ----no_noise_plot------------------------------------------------------------
  dim <- ncol(henon_embedded)
  p <- henon_embedded[ ,2:dim]
  t <- henon_embedded[ ,1]
  gamma_test(predictors = p, target = t, plot = TRUE, caption = "Deterministic function")


