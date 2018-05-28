week4_preload <- function() {
  library(dslabs)
  data("polls_us_election_2016")
}

poll_agg1 <- function() {
  d <- 0.039
  Ns <- c(1298,533,1342,897,774,254,812,324,1291,1056,2172,516)
  p <- (d+1)/2
  
  conf_int <- sapply(Ns, function(N) {
    X <- sample(c(0,1),size=N,replace=TRUE,prob=c(1-p,p))
    X_hat<- mean(X)
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    2*c(X_hat, X_hat-2*SE_hat, X_hat+2*SE_hat)-1
  })
  
  polls <- data.frame(poll=1:ncol(conf_int),
                      t(conf_int), sample_size=Ns)
  names(polls)<-c("polls","estimate","low","high","sample_size")
  
  sum(polls$sample_size)
  d_hat <- polls %>%
          summarize(avg=sum(estimate*sample_size)/sum(sample_size)) %>%
        .$avg
  p_hat <- (1+d_hat)/2
  moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
  moe
  round(d_hat*100,1)
  round(moe*100, 1)
}

ex_poll_bias <- function() {
  polls <- polls_us_election_2016 %>%
          filter(state=="U.S." & enddate>="2016-10-31" &
                   (grade %in% c("A+","A","A-","B+") |is.na(grade)))
  polls <- polls %>%
          mutate(spread=rawpoll_clinton/100-rawpoll_trump/100)
  d_hat <- polls %>%
          summarize(d_hat=sum(spread*samplesize)/sum(samplesize)) %>%
          .$d_dat
  p_hat <- (d_hat+1)/2
  moe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
  moe
  
  polls %>% ggplot(aes(spread)) +
      geom_histogram(color="black",binwidth=.01)
  # however, it does not look like normal distribution
  polls %>% group_by(pollster) %>%
      filter(n()>=6) %>%
      ggplot(aes(pollster,spread)) +
      geom_point() +
      theme(axis.text.x=element_text(angle=90,hjust=1))
  
}

data_driven_model <- function() {
  one_poll_per_pollster <- polls %>% group_by(pollster) %>%
                  filter(enddate==max(enddate)) %>%
                  ungroup()
  one_poll_per_pollster %>%
    ggplot(aes(spread)) + geom_histogram(binwidth=0.01)
  sd(one_poll_per_pollster$spread)
  # use central limit theorem
  results <- one_poll_per_pollster %>%
        summarize(avg=mean(spread),
                  se=sd(spread)/sqrt(length(spread))) %>%
        mutate(start=avg-1.96*se,end=avg+1.96*se)
  round(results*100,1)
}