rand1 <- function() {
    beads <- rep(c("red","blue"), times=c(2,3))
    x <- ifelse(sample(beads,1) == "blue", 1, 0)
    
}