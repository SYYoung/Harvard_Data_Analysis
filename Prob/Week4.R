ex4_1 <- function() {
  n <- 1000
  loss_per_foreclose <- 200000
  p <- 0.02
  defaults <- sample(c(0,1),n,prob=c(1-p,p),replace=TRUE)
  sum(defaults * loss_per_foreclose)
  
  # run  monte-carlo simulation
  B <- 10000
  losses <- replicate(B, {
          defaults <- sample(c(0,1),n,prob=c(1-p,p),replace=TRUE)
          sum(defaults*loss_per_foreclose)
  })
  data.frame(losses_in_millions=losses/10^6) %>%
    ggplot(aes(losses_in_millions)) +
    geom_histogram(binwidth=0.6,col="black")
  
  # by CLS
  avg = n *(p*loss_per_foreclose + (1-p)*0)
  se = sqrt(n)*abs(loss_per_foreclose) *sqrt(p*(1-p))
  # can use formulat to find the charge to the non-default
  #lp +x(1-p) = 0, find out x
  # therefore, x = -lp/(1-p)
  
  # now, we want to Prob of lose money is small, e.g. 0.01
  #P(S<0) = 0.01
}

ex4_2 <- function() {
  r <- 0.05
  x <- r*180000
  loss_per_foreclosure*p + x*(1-p)
  #Pr(S<0) = Pr(Z <(-E[S]/SE[S]))
  
  # now the events are not independent
  p <- 0.04
  x <- 0.05*180000
  profit <- replicate(B, {
      new_p <- 0.04 + sample(seq(-0.01,0.01,length=100),1)
      draws <- sample(c(x,loss_per_foreclosure),n,
                        prob=c(1-new_p,new_p),replace=TRUE)
      sum(draws)
  })
}

