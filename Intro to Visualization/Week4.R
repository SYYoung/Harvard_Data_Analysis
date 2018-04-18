loadWeek4 <- function() {
    library(dslabs)
    data("gapminder")
    head(gapminder)
    library(dplyr)
}

test1 <- function() {
    gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

}

ex_life1 <- function() {
    ds_theme_set()
    p <-filter(gapminder, year==2015) %>% ggplot(aes(fertility,life_expectancy,color=continent))
    p <- p + geom_point()
    
    p
}

ex_life2 <- function() {
    ds_theme_set()
    p <- filter(gapminder, year%in% c(1962,2012)) %>% ggplot(aes(fertility,life_expectancy,col=continent))
    p <- p + geom_point() 
    p1 <- p + facet_grid(continent~year)
    
    p1
    p2 <- p + facet_grid(.~year)
    p2
}

ex_life3 <- function() {
    years <- c(1962,1980,1990,2000,2012)
    continents <- c("Europe", "Asia")
    s <-gapminder %>% filter(year%in% years & continent %in% continents)
    p <- ggplot(s,aes(fertility,life_expectancy,col=continent))
    p <- p+ geom_point()
    p <- p + facet_wrap(~year)
    p
}

ex_time1 <- function() {
    s <- gapminder %>% filter(country=="United States")
    p <- ggplot(s,aes(year,fertility)) + geom_line()
    
    p
}

# display 2 lines, each for one country
ex_time2 <- function() {
    countries <- c("South Korea","Germany")
    s <- gapminder %>% filter(country %in% countries)
    p <- ggplot(s, aes(year, fertility,group=country,col=country))
    p <- p + geom_line()
    
    p
}

ex_time3 <- function() {
    countries <- c("South Korea", "Germany")
    labels <- data.frame(country=countries,x=c(1975,1965),y=c(60,72))
    s <- gapminder %>% filter(country %in% countries)
    p <- ggplot(s,aes(year,life_expectancy,col=country))
    p <- p + geom_line()
    p <- p + geom_text(data=labels,aes(x,y,label=country),size=5)
    p <- p + theme(legend.position="none")
    
    p
}

ex_trans1 <- function() {
    
}