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
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    past_year <- 1970
    s <- gapminder %>% filter(year==past_year & !is.na(gdp))
    p <- ggplot(s,aes(log2(dollars_per_day)) )
    p <- p + geom_histogram(binwidth=1,color="black")
    
    p
}

# instead of transform the data before plotting,
# we use the log scale on the axis
ex_trans2 <- function() {
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    past_year <- 2010
    s <- gapminder %>% filter(year==past_year & !is.na(gdp))
    p <- ggplot(s, aes(dollars_per_day)) 
    p <- p + geom_histogram(binwidth=1,color="black")
    p <- p + scale_x_continuous(trans="log2")
    
    p
}

ex_stratify <- function() {
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    past_year <- 2010
    s <- gapminder %>% filter(year==past_year & !is.na(gdp))
    p <- ggplot(s, aes(region, dollars_per_day)) 
    p <- p + geom_boxplot()
    p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
    
    p
}

ex_mean <- function() {
    fac <- factor(c("Asia","Asia","West","West","West"))
    levels(fac)
    value <- c(10,11,12,6,4)
    fac <- reorder(fac, value, FUN=mean)
    levels(fac)
}

ex_stratify2 <- function() {
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    
    past_year <- 1970
    s <- gapminder %>% filter(year==past_year & !is.na(gdp))
    s <- mutate(s,region=reorder(region,dollars_per_day,FUN=median))
    p <- ggplot(s, aes(region, dollars_per_day,fill=continent)) 
    p <- p + geom_boxplot()
    p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
    p <- p + scale_y_continuous(trans="log2")
    p <- p + xlab("")
    ## to show the indiv point for each country
    p <- p + geom_point(show.legend = FALSE)
    
    p
}

ex_comp_dist <- function() {
    west <- c("Western Europe", "Northern Europe", "Southern Europe")
    west <- c(west, "Northern America", "Australia and New Zealand")
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    past_year <- 1970
    s <- gapminder %>% filter(year==past_year & !is.na(gdp))
    s <- mutate(s, group=ifelse(region%in%west, "West", "Developing"))
    p <- ggplot(s, aes(dollars_per_day))
    p <- p + geom_histogram(binwidth=1,color="black")
    p <- p + scale_x_continuous(trans="log2")
    p <- p + facet_grid(.~group)
    
    p
}

ex_comp_dist2 <- function() {
    west <- c("Western Europe", "Northern Europe", "Southern Europe")
    west <- c(west, "Northern America", "Australia and New Zealand")
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    past_year <- 1970
    present_year <- 2010
    s <- gapminder %>% filter(year %in% c(past_year,present_year) & !is.na(gdp))
    s <- mutate(s, group=ifelse(region%in%west, "West", "Developing"))
    p <- ggplot(s, aes(dollars_per_day))
    p <- p + geom_histogram(binwidth=1,color="black")
    p <- p + scale_x_continuous(trans="log2")
    p <- p + facet_grid(year~group)
    
    p
}

ex_comp_dist3 <- function() {
    past_year <- 1970
    present_year <- 2010
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    country_list_1 <- gapminder %>% filter(year==past_year & !is.na(dollars_per_day)) %>% .$country
    country_list_2 <- gapminder %>% filter(year==present_year & !is.na(dollars_per_day)) %>% .$country
    country_list <- intersect(country_list_1, country_list_2)
    west <- c("Western Europe", "Northern Europe", "Southern Europe")
    west <- c(west, "Northern America", "Australia and New Zealand")
    
    s <- gapminder %>% filter(year %in% c(past_year,present_year) & country %in% country_list)
    s <- mutate(s, group=ifelse(region%in%west, "West", "Developing"))
    p <- ggplot(s, aes(dollars_per_day))
    p <- p + geom_histogram(binwidth=1,color="black")
    p <- p + scale_x_continuous(trans="log2")
    p <- p + facet_grid(year~group)
    
    p
}

ex_comp_dist4 <- function() {
    past_year <- 1970
    present_year <- 2010
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    country_list_1 <- gapminder %>% filter(year==past_year & !is.na(dollars_per_day)) %>% .$country
    country_list_2 <- gapminder %>% filter(year==present_year & !is.na(dollars_per_day)) %>% .$country
    country_list <- intersect(country_list_1, country_list_2)
    s <- gapminder %>% filter(year %in% c(past_year,present_year) & country %in% country_list)
    s <- mutate(s, region=reorder(region,dollars_per_day,FUN=median)) 
    p <- ggplot(s) 
    p <- p + theme(axis.text.x = element_text(angle=90,hjust=1))
    p <- p + xlab("") + scale_y_continuous((trans="log2"))
    p <- p + geom_boxplot(aes(region,dollars_per_day,fill=factor(year)))
    #p <- p + facet_grid(year~.)
    p
}

ex_density1 <- function() {
    past_year <- 1970
    present_year <- 2010
    gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)
    country_list_1 <- gapminder %>% filter(year==past_year & !is.na(dollars_per_day)) %>% .$country
    country_list_2 <- gapminder %>% filter(year==present_year & !is.na(dollars_per_day)) %>% .$country
    country_list <- intersect(country_list_1, country_list_2)
    west <- c("Western Europe", "Northern Europe", "Southern Europe")
    west <- c(west, "Northern America", "Australia and New Zealand")
    
    s <- gapminder %>% filter(year %in% c(past_year,present_year) & country %in% country_list)
    s <- mutate(s, group=ifelse(region%in%west, "West", "Developing"))
    p <- ggplot(s, aes(dollars_per_day, y= ..count.., fill=group)) 
    p <- p + scale_x_continuous(trans="log2")
    p <- p + geom_density(alpha=0.2) + facet_grid(year~.)
    
    p
}

ex_casewhen <- function() {
    west <- c("Western Europe", "Northern Europe", "Southern Europe")
    west <- c(west, "Northern America", "Australia and New Zealand")
    gapminder <- gapminder %>%
        mutate(group = case_when(
                gapminder$region %in% west ~ "The West",
              gapminder$region %in% "Northern Africa" ~ "Northern Africa",
              gapminder$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
              gapminder$region == "Southern Asia" ~ "Southern Asia",
              gapminder$region %in% c("Central America","South America","Caribbean") ~ "Latin America",
              gapminder$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
              gapminder$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

    present_year <- 2010
    surv_income <- gapminder %>%
        filter(year %in% present_year & !is.na(gdp) &
        !is.na(infant_mortality) & !is.na(group)) %>%
        group_by(group) %>%
        summarize(income=sum(gdp)/sum(population)/365,
                  infant_survival_rate=1-sum(infant_mortality/1000*population)/sum(population))
    surv_income %>% arrange(income)
    p <- surv_income %>% ggplot(aes(income,infant_survival_rate,label=group,color=group))
    p <- p + scale_x_continuous(trans="log2",limit=c(0.25,150)) 
    p <- p + scale_y_continuous(trans="logit",limit=c(0.875,0.9981),
                                breaks=c(0.85,0.90,0.95,0.99,0.995,0.998))
    p <- p + geom_label(size=3, show.legend = FALSE)
    
    p

}