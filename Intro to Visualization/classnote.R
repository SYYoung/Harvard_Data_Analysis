howToPlot <- function() {
    r <- murders %>% summarize(rate=sum(total)/sum(population) * 10^6) %>% .$rate
    
    p <- murders %>% ggplot(aes(population/10^6, total, label=abb))
    
    p <- p + geom_abline(intercept=log10(r), lty=2, color="darkgrey") 
    p <- p+ geom_point(aes(col=region), size=3)
    # since we have ggrepel, we use it instead of geom_text
    # p <- p + geom_text(nudge_x = 0.05)
    p <- p + geom_text_repel()
    p <- p + scale_x_log10() + scale_y_log10()
    p <- p + xlab("Pop in million (log10)")
    p <- p + ylab("total num of murders in log10")
    p <- p + ggtitle("US gun murders in US 2010")
    p <- p + scale_color_discrete(name="Region")
    
    p <- p + theme_economist()
    
    p
}

loadTheme <- function() {
    # install packages if not
    # install.packages("ggthemes")
    # install.packages("ggrepel")
    library(ggthemes)
    library(ggrepel)
    library(gridExtra)
}

addon <- function() {
    howToPlot()
}

plotHist <- function() {
    data(murders)
    p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
    p <- p + geom_histogram(binwidth = 1, fill="blue", col="black")
    p <- p + xlab("Male heights in inches")
    p <- p + ggtitle("Histogram")
    
    p
}

plotDensity <- function() {
    data(murders)
    p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
    p <- p + geom_density(fill="blue")
    p <- p + xlab("Male heights in inches")
    p <- p + ggtitle("Histogram")
    
    p
}

plotqq <- function() {
    data(murders)
    params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height),sd=sd(height))
    p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=scale(height)))
    p <- p + geom_qq(dparams=params)
    p <- p + xlab("Male heights in inches")
    p <- p + ggtitle("Histogram")
    p <- p + geom_abline()
    p
}

plotGrid <- function() {
    data(murders)
    p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
    p1 <- p + geom_histogram(binwidth=1,fill="blue", col="black")
    p2 <- p + geom_histogram(binwidth=2,fill="blue",col="black")
    p3 <- p + geom_histogram(binwidth=3,fill="blue",col="black")
    # assume library is installed: gridExtra
    grid.arrange(p1, p2, p3, ncol=3)
    
}
