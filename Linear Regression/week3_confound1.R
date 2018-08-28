ex3_spur_cor <- function() {
    N <- 25
    G <- 1000000
    # step1: construct random numbers
    sim_data <- tibble(group=rep(1:G,each=N), X=rnorm(N*G), Y=rnorm(N*G))
    # step2: 
    res <- sim_data %>%
        group_by(group) %>%
        summarize(r=cor(X,Y)) %>%
        arrange(desc(r))
    sim_data %>% 
        filter(group==res$group[which.max(res$r)]) %>%
        ggplot(aes(X,Y)) +
        geom_point() +
        geom_smooth(method="lm")
    res %>%
        ggplot(aes(x=r)) +
        geom_histogram(binwidth=0.1, color="black")
    # step3: we may mistakely think that they are closely related
    sim_data %>%
        filter(group==res$group[which.max(res$r)]) %>%
        do(tidy(lm(Y~X, data=.)))
    
}

ex3_outlier1 <- function() {
    set.seed(1)
    x <- rnorm(100, 100, 1)
    y <- rnorm(100, 84, 1)
    x[-23] <- scale(x[-23])
    y[-23] <- scale(y[-23])
    cor(x,y)
    cor(x[-23],y[-23])
    cor(rank(x), rank(y))
    cor(x, y, method="spearman")
    
}

ex3_reverse_init <- function() {
    library(HistData)
    data("GaltonFamilies")
    library(datasets)
    data("UCBAdmissions")
}

ex3_reverse_cause_effect1 <- function() {
    GaltonFamilies %>%
        filter(childNum==1 & gender == "male") %>%
        select(father, childHeight) %>%
        rename(son=childHeight) %>%
        do(tidy(lm(father~son, data=.)))
    
}

ex3_confound_1 <- function() {
    # need to arrange data as the example
    admissions <- as.data.frame(UCBAdmissions) %>%
        rename(gender=Gender) %>%
        rename(major=Dept) %>%
        rename(applicants=Freq) %>%
        mutate(admitted=ifelse(Admit=="Admitted",1,0))
    admissions %>% group_by(gender) %>%
        summarize(percentage=
                round(sum(admitted*applicants)/sum(applicants)*100,1))
    
    #by chi-sq test
    admissions %>% group_by(gender) %>%
        summarize(total_admitted=round(sum(admitted*applicants),1),
                  not_admitted=sum(applicants)-sum(total_admitted)) %>%
        select(-gender) %>%
        do(tidy(chisq.test(.)))
    
    admissions %>% select(major, gender, admitted, applicants) %>%
        filter(admitted==1) %>%
        spread(gender, applicants) %>%
        mutate(women_minus_men=Female-Male)
    
    # check whether the major selectivity is a cofounder
    admissions %>%
        group_by(major) %>%
        summarize(major_selectivity=sum(admitted*applicants)/sum(applicants),
            percent_women_applicants=sum(applicants*(gender=="Female")/sum(applicants))) %>%
            ggplot(aes(major_selectivity, percent_women_applicants,label=major)) +
            geom_text()
    # plots to show in bargraph
    admissions %>%
        mutate(percent_admitted=admitted*applicants/sum(applicants)) %>%
        ggplot(aes(gender, y=percent_admitted, fill=major)) +
        geom_bar(stat="identity", position="stack")
    
    admissions %>% 
        ggplot(aes(major,admitted*applicants, col=gender, size=applicants)) +
        geom_point()
    
    admissions %>% group_by(gender) %>%
        summarize(average=mean(admitted*applicants))
}

ex3_confound2 <- function() {
    admissions <- as.data.frame(UCBAdmissions) %>%
                rename(gender=Gender) %>%
                rename(major=Dept) %>%
                rename(applicants=Freq) %>%
                mutate(accept=ifelse(Admit=="Admitted",1,0))
    admissions %>% 
        group_by(major) %>%
        summarize(sum(accept*applicants)/sum(applicants))
    
    admissions
}