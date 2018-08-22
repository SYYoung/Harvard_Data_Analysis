ex2_init <- function() {
    library(Lahman)
    library(dslabs)
    library(tidyverse)
    library(HistData)
}

ex2 <- function() {
    ds_theme_set()
    Teams %>%
        filter(yearID %in% 1961:2001) %>%
        mutate(Singles=(H-HR-X2B-X3B)/G, BB=BB/G, HR=HR/G) %>%
    summarize(cor(BB,HR), cor(Singles, HR), cor(BB, Singles),
              cor(Singles, R/G)*sd(R/G)/sd(Singles))
}

# to determine if BB is still useful for creating runs. we
# fix the home run to see the relationship between B and run per game
ex2_strat1 <- function() {
    dat <- Teams %>%
        filter(yearID %in% 1961:2001) %>%
        mutate(HR_strata=round(HR/G,1),
               BB_per_game = BB/G,
               R_per_game = R/G) %>%
        filter(HR_strata >= 0.4 & HR_strata <= 1.2)
    dat %>% ggplot(aes(BB_per_game, R_per_game)) +
        geom_point(alpha=0.5) +
        geom_smooth(method="lm") +
        facet_wrap(~HR_strata)
    # see the slope in each facet
    dat %>%
        group_by(HR_strata) %>%
        summarize(slope=cor(BB_per_game, R_per_game) *
                      sd(R_per_game)/sd(BB_per_game))
    
}

ex2_strat2 <- function() {
    # check if stratfying base on ball, we still see a home run
    # effect or if it goes down
    dat <- Teams %>% 
        filter(yearID %in% 1961:2001) %>%
        mutate(BB_strata=round(BB/G, 1),
               HR_per_game = HR/G,
               R_per_game = R/G) %>%
        filter(BB_strata <= 2.8 & BB_strata <= 3.9)
    
    dat %>%
        ggplot(aes(HR_per_game, R_per_game)) +
        geom_point(alpha=0.5) +
        geom_smooth(method="lm") +
        facet_wrap(~BB_strata)
   # the slopes of each strata
    dat %>%
        group_by(BB_strata) %>%
        summarize(slope=cor(HR_per_game,R_per_game)*
                      sd(R_per_game)/sd(HR_per_game))

}