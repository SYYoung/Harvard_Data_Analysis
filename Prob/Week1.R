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

loadWeek1 <- function() {
  library(gtools)
}

permu1 <- function() {
  suits <- c("Diamonds","Clubs","Hearts","Spades")
  numbers <-c("Ace","Deuce","Three","Four","Five",
              "Six","Seven","Eight","Nine","Ten",
              "Jack","Queen","King")
  deck <- expand.grid(number=numbers,suit=suits)
  deck <- paste(deck$number,deck$suit)
  
  # calculate prob of a king
  kings <- paste("King",suits)
  mean(deck %in% kings)
  
  # now draw 2 card
  hands <- permutations(52, 2, v=deck)
  first_card <- hands[,1]
  second_card <- hands[,2]
  sum(first_card %in% kings & second_card %in% kings)/
      sum(first_card %in% kings)
}

comb_phone_num <- function() {
  all_phone_numbers <- permutations(10,7,v=0:9)
  n <- nrow(all_phone_numbers)
  index <- sample(n,5)
  all_phone_numbers[index,]
}

create_deck <- function() {
  suits <- c("Diamonds","Clubs","Hearts","Spades")
  numbers <-c("Ace","Deuce","Three","Four","Five",
              "Six","Seven","Eight","Nine","Ten",
              "Jack","Queen","King")  
}

combine1 <- function() {
  create_deck()
  deck <- expand.grid(number=numbers,suit=suits)
  # cal the prob of blackjack
  aces <- paste("Ace",suits)
  facecard <- c("King","Queen","Jack","Ten")
  facecard <- expand.grid(number=facecard,suit=suits)
  
  hands <- combinations(52,2,v=deck)
  mean(hands[,1] %in% aces & hands[,2] %in% facecard)
  
}

monte21 <- function() {
  create_deck()
  hand <- samples(deck, 2)
  B <- 10000
  results <- replicate(B, {
    hand <- sample(deck,2)
    (hand[1] %in% aces & hand[2] %in% facecard) |
      (hand[2] %in% aces & hand[1] %in% facecard)
  })
  mean(results)
}

same_birthday <- function() {
    n <- 50
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
    B <- 10000
    resuls <- replicate(B, {
        bdays <- sample(1:365, n, replace=TRUE)
        any(duplicated(bdays))
    })
    mean(resuls)
}

compute_prob <- function(n, B=10000) {
    same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace=TRUE)
        any(duplicated(bdays))
    })
    mean(same_day)
}

ex_sapply <- function() {
    n <- 1:70
    prob <- sapply(n, compute_prob)
    plot(n, prob)
}

exact_prob <- function(n) {
    prob_unique <- seq(365,365-n+1)/365
    1 - prod(prob_unique)
}

exact_prob_sapply <- function() {
    n <- 1:70
    eprob <- sapply(n, exact_prob)  
    ex_sapply()
    lines(x=n, y=eprob, type="l", col="red")
}

whichEnough <- function() {
    B <- 10^seq(1,5,len=100)
    compute_prob <- function(B, n=22) {
        same_day <- replicate(B, {
        bdays <- sample(1:365, n, replace=TRUE)
        any(duplicated(bdays))
        })
        mean(same_day)
    }
    prob <- sapply(B, compute_prob)
    plot(x=log10(B), y=prob, type="l")
}