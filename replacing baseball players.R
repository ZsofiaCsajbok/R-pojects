library(dplyr)


#CAPSTONE DATA PROJECT

#Finding replacement players for the three key players that the baseball team lost
#The players lost were: first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees, 
#outfielder Johnny Damon (damonjo01) to the Boston Red Sox and infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').
#Task:
#The total combined salary of the three players can not exceed 15 million dollars.
#Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#Their mean OBP had to equal to or greater than the mean OBP of the lost players

batting <- read.csv("F:/GitHub/Batting.csv",head=T)
str(batting)
head(batting$AB)
head(batting$X2B)

batting$batting_avg <- batting$H / batting$AB
batting$BA <- batting$H / batting$AB
tail(batting$batting_avg,5)

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$OBP <- (batting$H+batting$BB + batting$HBP)/(batting$AB+batting$BB + batting$HBP + batting$SF)
batting$SLG <- (batting$X1B + 2*batting$X2B + 3*batting$X3B + 4*batting$HR)/batting$AB

str(batting)

salaries <- read.csv("F:/GitHub/Salaries.csv",head=T)
summary(salaries)
batting1985 <- batting[which(batting$yearID>=1985),]
summary(batting1985)
combo <- merge(batting1985, salaries, by= c('playerID','yearID'))
summary(combo)
lostplayers_list <- c("giambja01", "damonjo01", "saenzol01")
lost_players <- subset(combo, playerID %in% lostplayers_list)
lost_players2001 <- subset(lost_players, yearID==2001)
lost_players2001_short <- lost_players2001 %>% select(playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB)

target_OBP <- mean(lost_players2001_short$OBP)  #0.3638687
target_AB <- sum(lost_players2001_short$AB)   #1469
target_salary <- 15000000

combo2001 <- subset(combo, yearID == 2001)
str(combo2001)
combo2001short <- combo2001 %>% select(playerID,OBP,AB,salary)
combo2001short_valid <- combo2001short %>% filter(complete.cases(OBP,AB)) %>% filter(salary <= 15000000) %>% filter(AB > 300) %>% filter(OBP > 0.2)

arranged_combo <- arrange(combo2001short_valid, desc(OBP), desc(AB), salary)
arranged_combo_first3 <- arranged_combo[1:3,]

combo2001short_valid$meanrows <- c(1:nrow(combo2001short_valid)) 
combo2001short_valid <- combo2001short_valid %>% select(playerID,OBP,AB,salary)


#Solution 1:
  
new_players <- function(t_OBP,t_AB,t_salary){
  repeat{
    sample_players <- combo2001short[sample(nrow(combo2001short),3),]
    a <- ifelse((mean(sample_players$OBP) >= t_OBP),1,0)
    b <- ifelse((sum(sample_players$AB) >= t_AB),1,0)
    c <- ifelse((sum(sample_players$salary) <= t_salary),1,0)
    result <- ifelse((is.na(sum(c(a,b,c)))),F,(sum(c(a,b,c))))
  if (result==3)
    break
    }
  return(sample_players)
}

new_players(0.3638687,1469,15000000)

#solution 2:

new_players <- function(t_OBP,t_AB,t_salary){
  success <- F
  while(success==F){
    sample_players <- combo2001short_valid[sample(nrow(combo2001short_valid),3),]
    a <- ifelse((mean(sample_players$OBP) >= t_OBP),1,0)
    b <- ifelse((sum(sample_players$AB) >= t_AB),1,0)
    c <- ifelse((sum(sample_players$salary) <= t_salary),1,0)
    result <- sum(a,b,c)
    success <- ifelse(result==3,T,F)
  }
  print(sample_players)
  print(mean(sample_players$OBP))
  print(sum(sample_players$AB))
  print(sum(sample_players$salary))
  return(sample_players$playerID)
}

new_players(0.3638687,1469,15000000)


