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