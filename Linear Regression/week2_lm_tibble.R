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