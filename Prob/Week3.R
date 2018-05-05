rand1 <- function() {
    beads <- rep(c("red","blue"), times=c(2,3))
    x <- ifelse(sample(beads,1) == "blue", 1, 0)
    
}

sec3_1 <- function() {
  color <- rep(c("Black","Red","Green"), c(18,18,2))
  n <- 1000
  X <- sample(ifelse(color=="Red", -1, 1), n, replace=TRUE)
  
  # we can rewrite the code in one line
  X <- sample(c(-1, 1), n, replace=TRUE, prob=c(9/19,10/19))
  S <- sum(X)
  
  B <- 10000
  S <- replicate(B, {
    X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19,10/19))
    sum(X)
  })
  mean(S <= 0)
  hist(S, freq=FALSE)
  
  # use this to generate a normal dist
  s <- seq(min(S), max(S), length=100)
  normal_den = data.frame(s=s, f=dnorm(s,mean(S),sd(S)))
  data.frame(S=S) %>% ggplot(aes(S, ..density..)) +
            geom_histogram(color="black",binwidth=10) +
            ylab("Probablilty") +
            geom_line(data=normal_den, mapping=aes(s,f),color="blue")
  
}