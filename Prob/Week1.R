fun1 <- function() {
  beads <- rep(c("red","blue"), times=c(2,3))
  B <- 100000
  events <- replicate(B, sample(beads, 1))
  tab <- table(events)
  tab
  prop.table(tab)
  
  # with replacement
  events <- sample(beads, B, replace=TRUE)
  prop.table(table(events))
}