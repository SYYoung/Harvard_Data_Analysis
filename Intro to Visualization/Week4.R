loadWeek4 <- function() {
    library(dslabs)
    data("gapminder")
    head(gapminder)
}

test1 <- function() {
    gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

}