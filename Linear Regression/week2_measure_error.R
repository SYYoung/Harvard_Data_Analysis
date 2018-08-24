ex2_measure_init <- function() {
    library(dslabs)
    library(broom)
}

ex2_measure_err1 <- function() {
    falling_object <- rfalling_object()
    falling_object <- falling_object %>%
        rename(y=observed_distance)
    falling_object %>%
        ggplot(aes(time, y=y)) +
        geom_point() +
        ylab("Distance in meters") +
        xlab("Time in seconds")
    fit <- falling_object %>%
        mutate(time_sq = time^2) %>%
        lm(y~time+time_sq, data=.)
    tidy(fit)
    
    augment(fit) %>%
        ggplot() +
        geom_point(aes(time, y)) +
        geom_line(aes(time, .fitted))
    tidy(fit, conf.int=TRUE)
    
}