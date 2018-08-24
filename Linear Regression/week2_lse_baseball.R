ex2_base1 <- function() {
    fit <- Teams %>%
        filter(yearID %in% 1961:2001) %>%
        mutate(BB=BB/G, HR=HR/G, R=R/G) %>%
        lm(R~ BB+HR, data=.)
    tidy(fit, conf.int=TRUE)
    
    # now we consider variables: single,double,triples,HR,BB
    fit <- Teams %>%
        mutate(BB=BB/G,
               singles=(H-X2B-X3B-HR)/G,
               doubles=X2B/G,
               triples=X3B/G,
               HR=HR/G,
               R=R/G) %>%
        lm(R~BB+singles+doubles+triples+HR, data=.)
    tidy(fit, conf.int=TRUE)
    
    # now we use this prediction to predict year 2002
    Teams %>%
        filter(yearID %in% 2002) %>%
        mutate(BB=BB/G,
               singles=(H-X2B-X3B-HR)/G,
               doubles=X2B/G,
               triples=X3B/G,
               HR=HR/G,
               R=R/G) %>%
        mutate(R_hat=predict(fit, newdata=.)) %>%
        ggplot(aes(R_hat, R)) +
            geom_point() + 
            geom_line(aes(R_hat,R_hat))
}

ex2_base2 <- function() {
    # it is to make per-game team rate comparable to the per-plate
    # appearance player rate
    pa_per_game <- Batting %>%
        filter(yearID==2002) %>%
        group_by(teamID) %>%
        summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
        .$pa_per_game %>%
        mean
    pa_per_game
}

ex2_base3 <- function() {
    pa_per_game = ex2_base2()
    players <- Batting %>%
        filter(yearID %in% 1999:2001) %>%
        group_by(playerID) %>%
        mutate(PA=BB+AB) %>%
        summarize(G=sum(PA)/pa_per_game,
                  BB=sum(BB)/G,
                singles=sum(H-X2B-X3B-HR)/G,
                doubles=sum(X2B)/G,
                triples=sum(X3B)/G,
                HR=sum(HR)/G,
                AVG=sum(H)/sum(AB),
                PA=sum(PA)) %>%
        filter(PA >=300) %>%
        select(-G) %>%
    mutate(R_hat=predict(fit, newdata=.))
    players %>% ggplot(aes(R_hat)) +
        geom_histogram(binwidth=0.5, color="black")
    players
}

ex2_base4 <- function() {
    players <- ex2_base3()
    players <- Salaries %>%
        filter(yearID==2002) %>%
        select(playerID, salary) %>%
        right_join(players, by="playerID")
    
    players <- Fielding %>%
        filter(yearID ==2002) %>%
        filter(!POS %in% c("OF","P")) %>%
        group_by(playerID) %>%
        top_n(1,G) %>%
        filter(row_number(G) ==1) %>%
        ungroup() %>%
        select(playerID, POS) %>%
        right_join(players, by="playerID") %>%
        filter(!is.na(POS) & !is.na(salary))
    
    players <- Master %>%
        select(playerID, nameFirst, nameLast, debut) %>%
        right_join(players, by="playerID")
    
    players %>% 
        select(nameFirst, nameLast, POS, salary, R_hat) %>%
        arrange(desc(R_hat)) %>%
        top_n(10)
    players %>% 
        ggplot(aes(salary, R_hat, color=POS)) +
        geom_point() +
        scale_x_log10()
    
}

ex2_reg_fall_1 <- function() {
    playerInfo <- Fielding %>%
        group_by(playerID) %>%
        arrange(desc(G)) %>%
        slice(1) %>%
        ungroup %>%
        left_join(Master, by="playerID") %>%
        select(playerID, nameFirst, nameLast, POS)
    
    ROY <- AwardsPlayers %>%
        filter(awardID == "Rookie of the Year") %>%
        left_join(playerInfo, by="playerID") %>%
        rename(rookie_year = yearID) %>%
        right_join(Batting, by="playerID") %>%
        mutate(AVG=H/AB) %>%
        filter(POS != "P")
    
    ROY <- ROY %>%
        filter(yearID == rookie_year | yearID==rookie_year+1) %>%
        group_by(playerID) %>%
        mutate(rookie=ifelse(yearID==min(yearID), "rookie","sophmore")) %>%
        filter(n()==2) %>%
        ungroup %>%
        select(playerID, rookie_year, rookie, nameFirst, nameLast,AVG)
    
    ROY <- ROY %>%
        spread(rookie, AVG) %>%
        arrange(desc(rookie))
    ROY
    
    # now we will look other players to see if the problem
    # also exists for other players
    two_years <- Batting %>%
        filter(yearID %in% 2013:2014) %>%
        group_by(playerID, yearID) %>%
        filter(sum(AB) >= 130) %>%
        summarize(AVG=sum(H)/sum(AB)) %>%
        ungroup %>%
        spread(yearID, AVG) %>%
        filter(!is.na('2013') & is.na('2014')) %>%
        left_join(playerInfo, by="playerID") %>%
        filter(POS != "P") %>%
        select(-POS) %>%
        arrange(desc('2013')) %>%
        select(-playerID)
    
    head(two_years)
    
    #arrange(two_years, '2013') # the worst performers on 2013
    #two_years %>%
     #   ggplot(aes(`2013`, `2014`)) +
      #  geom_point()
    #summarize(two_years, cor('2013', '2014'))

}

ex2_reg_fall_2 <- function() {
    playerInfo <- Fielding %>%
        group_by(playerID) %>%
        arrange(desc(G)) %>%
        slice(1) %>%
        ungroup %>%
        left_join(Master, by="playerID") %>%
        select(playerID, nameFirst, nameLast, POS)
    
    ROY <- AwardsPlayers %>%
        filter(awardID == "Rookie of the Year") %>%
        left_join(playerInfo, by="playerID") %>%
        rename(rookie_year = yearID) %>%
        right_join(Batting, by="playerID") %>%
        mutate(AVG=H/AB) %>%
        filter(POS != "P")

    ROY <- ROY %>%
        filter(yearID == rookie_year | yearID==rookie_year+1) %>%
        group_by(playerID) %>%
        mutate(rookie=ifelse(yearID==min(yearID), "rookie","sophmore")) %>%
        filter(n()==2) %>%
        ungroup %>%
        select(playerID, rookie_year, rookie, nameFirst, nameLast,AVG)
  
    ROY <- ROY %>%
        spread(rookie, AVG) %>%
        arrange(desc(rookie))

    # now we will look other players to see if the problem
    # also exists for other players
    two_years <- Batting %>%
        filter(yearID %in% 2013:2014) %>%
        group_by(playerID, yearID) %>%
        filter(sum(AB) >= 130) %>%
        summarize(AVG=sum(H)/sum(AB)) %>%
        ungroup %>%
        spread(yearID, AVG) %>%
        filter(!is.na(`2013`) & !is.na(`2014`)) %>%
        left_join(playerInfo, by="playerID") %>%
        filter(POS != "P") %>%
        select(-POS) %>%
        arrange(desc(`2013`)) %>%
        select(-playerID)

    arrange(two_years, `2013`) # the worst performers on 2013
    two_years %>%
        ggplot(aes(`2013`, `2014`)) +
        geom_point()
    summarize(two_years, cor(`2013`, `2014`))
    
}