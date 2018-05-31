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
    
}