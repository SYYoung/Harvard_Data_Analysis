load_week6 <- function() {
  library(dslabs)
  data("polls_us_election_2016")
  
}

elec_forcast1 <- function() {
  # mu is to describe the spread parameter. 
  mu <- 0
  tau <- 0.035
  sigma <- results$avg
  Y <- result$avg
  B <- sigma^2 /(sigma^2 + tau^2)
  
  posterior_mean <- B*mu + (1-B)*Y
  posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))
  posteria_mean
  posteria_se
  
  # credible interval:
  ci <- posterior_mean + c(-1.96,1.96)*posterior_se
  # the prob that the spread is bigger than 0
  1 - pnorm(0, posteria_mean, posterior_se)
  
}

elec_forc_model1 <- function() {
  # the model is: Xj = d + ej
  # Xij: the ith pollster of the jth poll
  # Xij = d + eij
  I <- 5 # number of pollster
  J <- 6
  N <- 2000
  d <- 0.021
  p <- (d+1)/2
  X <- sapply(1:I, function(i) {
    d + rnorm(J,0,2*sqrt(p*(1-p)/N))
  })
  
  # now we account the variability of the pollster
  # Xij = d + hi + eij
  # now we include hi in the simulation
  h <- rnorm(I, 0, 0.025)
  X <- sapply(1:I, function(i) {
    d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
  })
  
  # now we need to add the general bias variability
  # Xij = d + b + hi + eij where b is the general bias
  # b ~ N(0, 0.025) from historical data
  # X_bar = d + b + 1/N sum(Xi)
  # sd of X_bar = sqrt(sigma^2/N + sigma_b^2)
  # now taking everything into account, our result is
  # close to FiveThirtyEight
  mu <- 0
  tau <- 0.035
  sigma <= sqrt(results$se^2 + 0.025^2)
  Y <- results$avg
  B <- sigma^2 / (sigma^2 + tau^2)
  
  posterior_mean <- B*mu + (1-B)*Y
  posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))
  
  1 - pnorm(0, posterior_mean, posterior_se)
}