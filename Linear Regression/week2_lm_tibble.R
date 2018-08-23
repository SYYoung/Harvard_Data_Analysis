ex2_tibb1 <- function() {
    ex2_init()
    dat <- Teams %>%
        filter(yearID %in% 1961:2001) %>%
        mutate(HR=round(HR/G, 1), BB=BB/G, R=R/G) %>%
        select(HR, BB, R) %>%
        filter(HR >=0.4 & HR<=1.2)
    dat %>% group_by(HR) %>%
        lm(R ~ BB, data=.) %>%
        .$coef
    dat
}

ex2_tibb_example <- function() {
    tibble(id=c(1,2,3), func=c(mean,median,sd))
    
}

get_slope <- function(data) {
    fit <- lm(R~BB, data=data)
    data.frame(slope=fit$coefficents[2],
               se=summary(fit)$coefficent[2,2])
}

build_team <- function() {
    ex2_init()
    dat <- Teams %>%
        filter(yearID %in% 1961:2001) %>%
        mutate(HR=round(HR/G,1), BB=BB/G, R=R/G) %>%
        select(HR, BB, R) %>%
        filter(HR >=0.4 & HR<=1.2)    
    dat
}

ex2_tibb2 <- function() {
    dat <- build_team()
    dat %>% 
        group_by(HR) %>%
        do(get_slope(.))
    
}

get_lse <- function(data) {
    fit <- lm(R~BB, data=data)
    data.frame(team=names(fit$coefficients),
               slope=fit$coefficients,
               se=summary(fit)$coefficient[,2])
}

ex2_tibb3 <- function() {
    dat <- build_team()
    dat %>%
        group_by(HR) %>%
        do(get_lse(.))
    
}

