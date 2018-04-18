week3_dpylr <- function() {
    s <- heights %>% filter(sex=="Male") %>% summarize(average=mean(height), stdev=sd(height))
    s
    murd_rate <- murders %>% summarize(rate=sum(total)/sum(population) * 100000) 
    class(murd_rate)
    murd_rate
    murd_rate <- murders %>% summarize(rate=sum(total)/sum(population) * 100000) %>% .$rate
    class(murd_rate)
    murd_rate
   }

ex_groupby <- function() {
    heights %>% group_by(sex) %>% summarize(avg=mean(height),stdev=sd(height))
    murders <- mutate(murders, murder_rate = total/population *100000)
    murders %>% group_by(region) %>% summarize(median(murder_rate))
}

ex_sort <- function() {
    murders <- mutate(murders, mrate=total/population *10^5)
    murders %>% arrange(population) %>% head()  
    murders %>% arrange(desc(population)) %>% head()
    murders %>% arrange(region,mrate) %>% head()
    murders %>% top_n(10, mrate)
    murders %>% arrange(mrate) %>% top_n(10)
}

loadLib <- function() {
    library(tidyverse)
    data(heights)
}

loadLibHealth <- function() {
    library(NHANES)
    data(NHANES)
    mean(na_example, na.rm=TRUE)
    sd(na_example, na.rm=TRUE)
}