F <- function(a) mean(x<=a)

get_height <- function() {
    x <- heights %>% filter(sex=="Male") %>% .$height
    n <- length(x)
    avg <- mean(x)
    s <- sd(x) 
    c(n, avg, s)
}
monte_ht <- function() {
    get_height()
    sim_height <- rnorm(n, avg, s)
    ds_theme_set()
    data.frame(sim_height=sim_height) %>% ggplot(aes(sim_height)) +
            geom_histogram(color="black",binwidth=2)
}
how_7ft <- function() {
     d <- get_height()
     B <- 10000
     tallest <- replicate(B, {
         sim_data <- rnorm(800, d[2], d[3])
         max(sim_data)
     })
     mean(tallest >= 7*12)
 }
 
cont_dist <- function() {
    x <- seq(-4, 4, length.out=100)
    data.frame(x, f=dnorm(x)) %>% ggplot(aes(x,f)) +
        geom_line()
} 