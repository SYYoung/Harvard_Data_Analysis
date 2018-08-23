ex2_init <- function() {
    library(Lahman)
    library(dslabs)
    library(tidyverse)
    library(HistData)
}

ex2_galton_init <- function() {
    data("GaltonFamilies")
    galton_heights <- GaltonFamilies %>%
        filter(childNum == 1 & gender == "male") %>%
        select(father, childHeight) %>%
        rename(son = childHeight) 
    galton_heights
}

ex2lse_1 <- function() {
    galton_heights <- ex2_galton_init()
    rss <- function(beta0, beta1, data) {
        resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
        return(sum(resid^2))
    }
    # this is a 3-D.to simplify problem we fix beta0=25. to find
    # min RSS and beta1
    beta1 = seq(0, 1, len=nrow(galton_heights))
    results <- data.frame(beta1=beta1,
                          rss=sapply(beta1,rss,beta0=36))
    results %>% ggplot(aes(beta1, rss)) +
                geom_line() +
        geom_line(aes(beta1, rss), col=2)
    filter(results, rss==min(rss))
}

ex2lse_2 <- function() {
    galton_heights <- ex2_galton_init()
    fit <- lm(son ~ father, data=galton_heights)
    fit
    summary(fit)
    
}

ex2lse_q1 <- function() {
    ex2_init()
    t1 <- Teams %>%
        filter(yearID %in% 1961:2001) %>%
        mutate(RG=R/G, BBG=BB/G, HRG=HR/G)
    fit <- lm(RG ~ BBG+HRG, data=t1)
    fit
}

ex2lse_rv <- function() {
    galton_heights <- ex2_galton_init()
    B <- 1000
    N <- 50
    lse <- replicate(B, {
        sample_n(galton_heights, N, replace=TRUE) %>%
        lm(son ~ father, data=.) %>% .$coef
    })
    
    lse <- data.frame(beta_0=lse[1,], beta_1 = lse[2,])
    head(lse)
    library(gridExtra)
    p1 <- lse %>% ggplot(aes(beta_0)) +
        geom_histogram(binwidth=5, color="black")
    p2 <- lse %>% ggplot(aes(beta_1)) +
        geom_histogram(binwidht=0.1, color="black")
    grid.arrange(p1, p2, ncol=2)
    
    lse %>% summarize(se_0 = sd(beta_0), se_1=sd(beta_1))
    lse %>% summarize(cor(beta_0, beta_1))
}

ex2lse_rv2 <- function() {
    galton_heights <- ex2_galton_init()
    B <- 1000
    N <- 50
    lse <- replicate(B, {
        sample_n(galton_heights, N, replace=TRUE) %>%
        mutate(father=father-mean(father)) %>%
        lm(son ~ father, data=.) %>% .$coef
    })
    cor(lse[1,], lse[2,])
}

ex2lse_pred1 <- function() {
    galton_heights <- ex2_galton_init()
    galton_heights %>% ggplot(aes(son, father)) +
        geom_point() +
        geom_smooth(method="lm")
    galton_heights %>%
        mutate(Y_hat=predict(lm(son~father, data=.))) %>%
        ggplot(aes(father, Y_hat)) +
        geom_line()
    
}

ex2lse_pred2 <- function() {
    galton_heights <- ex2_galton_init()
    fit <- galton_heights %>% lm(son~father, data=.)
    Y_hat <- predict(fit, se.fit=TRUE)
    names(Y_hat)
    head(Y_hat)
    
}
