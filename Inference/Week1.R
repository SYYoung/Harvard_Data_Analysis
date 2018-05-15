load_week1 <- function() {
    library(tidyverse)
    library(dslabs)
    ds_theme_set()
    library(gridExtra)
}

fn1 <- function() {
    take_poll(25)
    # estimate: summary of the observed data that we think
    # is informative about the parameter of interest
    # we get 13 blue, 12 red
    X_hat <- 0.48
    se <- sqrt(X_hat * (1-X_hat)/25)
    se
    # check the prob that p is within 0.01
    pnorm(0.01/se) - pnorm(-0.01/se)
}

fn_monte_CLT <- function() {
    # however, we don't know,we need to find estimated p
    # run one experiment and get estimated p
    p <- 0.45
    N <- 1000
    X <- sample(c(0,1),size=N, replace=TRUE, prob=c(1-p,p))
    X_hat <- mean(X)
    
    B <- 10000
    N <- 1000
    X_hat <- replicate(B, { 
        X <- sample(c(0,1),size=N,replace=TRUE,prob=c(1-p,p))
        mean(X)
    })
    
    # draw the plot
    p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) +
            geom_histogram(binwidth=0.005,color="black")
    p2<- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=x_hat)) +
            stat_qq(dparams=list(mean=mean(X_hat),sd=sd(X_hat))) +
            geom_abline() +
            ylab("X_hat") +
            xlab("Theoretical normal")
    grid.arrange(p1,p2,nrow=1)
}