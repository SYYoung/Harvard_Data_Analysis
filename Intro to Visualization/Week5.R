loadWeek5 <- function() {
    library(dslabs)
    data("gapminder")
    head(gapminder)
    library(dplyr)
}

slopeChart <- function() {
    west <- c("Western Europe", "Northern Europe", 
              "Southern Europe", "Northern America",
              "Australia and New Zealand")
    dat <- gapminder %>% 
            filter(year %in% c(2010,2015) &
            region %in% west &
            !is.na(life_expectancy) &
            population > 10^7)
    dat %>%
        mutate(location = ifelse(year==2010,1,2),
               location=ifelse(year==2015 & 
                        country %in%c("United Kingdom","Portugal"),
                        location+0.22,location),
               hjust=ifelse(year==2010,1,0)) %>%
        mutate(year=as.factor(year)) %>%
        ggplot(aes(year,life_expectancy,group=country)) +
        geom_line(aes(color=country),show.legend=FALSE) +
        geom_text(aes(x=location,label=country,hjust=hjust),
                  show.legend=FALSE) +
        xlab("") + ylab("Life expectancy")
}

load_vaccine<- function() {
    data("us_contagious_diseases")
    library(RColorBrewer)
    display.brewer.all(type="seq")
}

ex_vaccine<- function() {
    the_disease <- "Measles"
    dat <- us_contagious_diseases %>%
            filter(!state %in% c("Hawaii","Alaska") & disease==the_disease) %>%
            mutate(rate=count/population *10000) %>%
            mutate(state=reorder(state,rate))
    # just to slow CA
    p <- dat %>% filter(state=="California") %>%
            ggplot(aes(year,rate)) +
            geom_line() + ylab("Cases per 10K") +
            geom_vline(xintercept=1963,col="blue")
    p
}

ex_vaccine2<- function() {
    the_disease <- "Measles"
    dat <- us_contagious_diseases %>%
        filter(!state %in% c("Hawaii","Alaska") & disease==the_disease) %>%
        mutate(rate=count/population *10000) %>%
        mutate(state=reorder(state,rate))
    # show all states, rate and year
    #display.brewer.all(type="seq")
    p <- dat %>% ggplot(aes(year, state, fill=rate)) +
                geom_tile(color="grey50") +
                scale_x_continuous(expand=c(0,0)) +
                scale_fill_gradientn(colors=brewer.pal(9,"Reds"),trans="sqrt") +
                geom_vline(xintercept=1963,col="blue") +
                theme_minimal() +
                theme(panel.grid=element_blank()) +
                ggtitle(the_disease) +
                ylab("") + xlab("")
    p
}

ex_vaccine3 <- function() {
    the_disease <- "Measles"
    dat <- us_contagious_diseases %>%
        filter(!state %in% c("Hawaii","Alaska") & disease==the_disease) %>%
        mutate(rate=count/population *10000) %>%
        mutate(state=reorder(state,rate))
    avg <- dat %>%
            filter(disease==the_disease) %>% group_by(year) %>%
            summarize(us_rate=sum(count,na.rm=TRUE)/sum(population,na.rm=TRUE) *10000)
    avg
    p <- dat %>% ggplot() + 
        geom_line(aes(year,rate,group=state),color="grey50",
                  show.legend=FALSE, alpha=0.2,size=1) +
        geom_line(mapping=aes(year,us_rate),data=avg,size=1,color="black") +
        scale_y_continuous(trans="sqrt",breaks=c(5,25,125,300)) +
        ggtitle("Cases per 10,000 by state") +
        xlab("") +
        ylab("") +
        geom_vline(xintercept=1963,col="blue") +
        geom_text(data=data.frame(x=1955,y=50),mapping=aes(x,y,label="US average rate"))
    p    
}