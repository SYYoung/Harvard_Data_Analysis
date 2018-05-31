lib_week7 <- function() {
    library(dslabs)
    data("research_funding_rates")
}

assoc_1 <- function() {
    totals <- research_funding_rates %>%
            select(-discipline) %>%
            summarize_all(funs(sum)) %>%
            summarize(yes_men=awards_men,
                      no_men=applications_men-awards_men,
                      yes_women=awards_women,
                      no_women=applications_women-awards_women)
    totals %>% summarize(percent_men=yes_men/(yes_men+no_men),
                         percent_women=yes_women/(yes_women+no_women))
    
    tab <- matrix(c(3,1,1,3),2,2)
    rownames(tab) <- c("Poured Before","Poured After")
    colnames(tab) <- c("Guessed Before","Guessed After")
    tab
    fisher.test(tab, alternative="greater")
    totals
}

chi_square <- function() {
    totals <- assoc_1()
    totals
    funding_rate <- totals %>%
        summarize(percent_total=
                      (yes_men+yes_women)/
                      (yes_men+no_men+yes_women+no_women)) %>%
        .$percent_total
    funding_rate
    
    # use chi-squared test to find out
    two_by_two <- tibble(awarded=c("no","yes"),
                         men=c(totals$no_men,totals$yes_men),
                         women=c(totals$no_women,totals$yes_women))
    two_by_two
    # compared above table with the overate
    tibble(awarded=c("no","yes"),
           men=(totals$no_men + totals$yes_men) *
               c(1-funding_rate, funding_rate),
           women=(totals$no_women + totals$yes_women) *
               c(1-funding_rate, funding_rate))
    two_by_two %>%
        select(-awarded) %>%
        chisq.test()
    # the odds = P(Y|X)/P(~Y|X), X=1 if male, Y=1 if funded
    odd_men <- (two_by_two$men[2]/sum(two_by_two$men)) /
        (two_by_two$men[1]/sum(two_by_two$men))
    odd_women<- (two_by_two$women[2]/sum(two_by_two$women))/
        (two_by_two$women[1]/sum(two_by_two$women))
    
    # small p-value does not imply big odd ratio
    two_by_two %>%
        select(-awarded) %>%
        mutate(men=men*10, women=women*10) %>%
        chisq.test()
    
}