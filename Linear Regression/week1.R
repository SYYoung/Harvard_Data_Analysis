ex1_init <- function() {
    library(Lahman)
    library(dslabs)
    library(tidyverse)
    library(HistData)
}

ex1 <- function() {
    ds_theme_set()
    Teams %>% filter(yearID %in% 1961:2001) %>%
        #mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
        #ggplot(aes(HR_per_game, R_per_game)) +
        #mutate(SB_per_game = SB/G, R_per_game=R/G) %>%
        #ggplot(aes(SB_per_game, R_per_game)) +
        mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
        ggplot(aes(BB_per_game, R_per_game)) +
        geom_point(alpha=0.5)
}

ex1_galton_init <- function() {
    data("GaltonFamilies")
    galton_heights <- GaltonFamilies %>%
        filter(childNum == 1 & gender == "male") %>%
        select(father, childHeight) %>%
        rename(son = childHeight) 
    galton_heights
}

ex1_galton <- function() {
    galton_heights <- ex1_galton_init()
    galton_heights %>%
        summarize(mean(father), sd(father), mean(son), sd(son))
    galton_heights %>% ggplot(aes(father, son)) +
        geom_point(alpha=0.5)
    
    # calculate the correlation between father and son
    galton_heights %>% summarize(cor(father, son))
}

ex1_sample <- function() {
    galton_heights <- ex1_galton_init()
    set.seed(0)
    R <- sample_n(galton_heights, 25, replace=TRUE) %>%
            summarize(cor(father, son))
    # run monte-carlo simulation
    B <- 1000
    N <- 25
    R <- replicate(B, {
        sample_n(galton_heights, N, replace=TRUE) %>%
            summarize(cor(father, son)) %>% .$r
    })
    data.frame(R) %>% ggplot(aes(R)) +
        geom_histogram(binwidth=0.05, color="black")
    
}

ex1_galton_strat <- function() {
    galton_heights <- ex1_galton_init()
    conditional_avg <- galton_heights %>%
        filter(round(father) == 72) %>%
        summarize(avg=mean(son)) %>% .$avg
    conditional_avg
    
    galton_heights %>% 
        mutate(father_strata = factor(round(father))) %>%
        ggplot(aes(father_strata, son)) +
        geom_boxplot() +
        geom_point()
    
    galton_heights %>%
        mutate(father=round(father)) %>%
        group_by(father) %>%
        summarize(son_conditional_avg = mean(son)) %>%
        ggplot(aes(father, son_conditional_avg)) +
        geom_point()
    
    r <- galton_heights %>%
            summarize(r=cor(father,son)) %>% .$r
    galton_heights %>%
        mutate(father=round(father)) %>%
        group_by(father) %>%
        summarize(son=mean(son)) %>%
        mutate(z_father=scale(father), z_son=scale(son)) %>%
        ggplot(aes(z_father, z_son)) +
        geom_point() +
        geom_abline(intercept=0, slope=r)
    
    mu_x <- mean(galton_heights$father)
    mu_y <- mean(galton_heights$son)
    sd_x <- sd(galton_heights$father)
    sd_y <- sd(galton_heights$son)
    r <- cor(galton_heights$father, galton_heights$son)
    m <- r * sd_y/sd_x
    b <- mu_y - m* mu_x
    galton_heights %>%
        ggplot(aes(scale(father), scale(son))) +
        geom_point(alpha=0.5) +
        geom_abline(intercept=0, slope=r)
    
}

ex1_bivar1 <- function() {
    galton_heights <- ex1_galton_init()
    galton_heights %>%
        mutate(z_father=round((father-mean(father))/sd(father))) %>%
        filter(z_father %in% -2:2) %>%
        ggplot() +
        stat_qq(aes(sample=son)) +
        facet_wrap(~z_father)
    
}

ex1_2reg <- function() {
    galton_heights <- ex1_galton_init()
    mu_x <- mean(galton_heights$father)
    mu_y <- mean(galton_heights$son)
    sd_x <- sd(galton_heights$father)
    sd_y <- sd(galton_heights$son)
    r <- cor(galton_heights$father, galton_heights$son)
    m <- r * sd_x/sd_y
    b <- mu_x - m * mu_y
}