# R packages
library(tidyverse)
library(kableExtra)
library(mosaic)
library(readxl)
library(surveillance)
library(knitcitations)

# table formatting function
mykable <- function(df){   
  kable(df) %>% 
  kable_styling("striped", full_width = FALSE)
}

# function to import data
epl.season <- function(year){
  epl.path <- paste("data/", year, ".csv", sep = "") # get file path
  epl.Getdata <- read.csv(epl.path) # read in file
  epl.Getdata <- epl.Getdata %>% 
    mutate(Season = paste(year , "-", year + 1, sep = "")) %>% 
    rename(Home.Goals = FTHG, Away.Goals = FTAG) %>% 
    select(Season, HomeTeam, AwayTeam, Home.Goals, Away.Goals) 
}  

# combine every season data into a table
epl.fulldata <- epl.season(1992) # initializes full dataset
for (year in 1993:2018) { # for loop
  epl.fulldata <- epl.fulldata %>%
    full_join(epl.season(year))
}

# quick look at table
epl.fulldata %>% 
  tail(4) %>%  
  mykable()

#__________________________________________________
# Poisson and number of goals

# get all Man Utd's home and away games
MUHome <- epl.fulldata %>%   
  filter(HomeTeam == "Man United") %>%  
  select(Home.Goals) %>% 
  mutate(type = "Home") %>% 
  rename(goals = Home.Goals) # rename for joining purposes

MUAway <- epl.fulldata %>%
  filter(AwayTeam == "Man United") %>% 
  select(Away.Goals) %>% 
  mutate(type = "Away") %>% 
  rename(goals = Away.Goals)

# joins MU's home and away data
MUGoals <- full_join(MUHome, MUAway, by = c("goals","type"))

# histogram of number of goals
MUGoals %>% 
  ggplot(aes(x = goals)) +
  geom_histogram(color = "darkgreen", fill = "lightgreen", bins = 10) +
  scale_x_continuous(breaks= 0:9)

# basic descriptive statistics of number of goals
fav_stats(MUGoals$goals) %>% 
  mykable()

# get mean, sd, variance
MeanGoals <- fav_stats(MUGoals$goals)[[6]]
numMatches <- fav_stats(MUGoals$goals)[[8]]
StDevGoals <- fav_stats(MUGoals$goals)[[7]]
VarianceGoals <- StDevGoals ^ 2

# mean = 1.916, var = 1.975. Pretty close!

# get number of matches having each possible goal value
GoalsTable <- 
  MUGoals %>% 
  group_by(goals) %>% 
  summarise(ActualMatches = n())
GoalsTable %>% 
  mykable()

# put goal values 4 to 9 into a "4 or more" category

# select first 4 rows (0, 1, 2, 3 goals)
NewGoalsTable <- GoalsTable[1:4,]
# sum up the remaining rows
NewGoalsTable[5,] <- sum(GoalsTable[5:nrow(GoalsTable),2])
NewGoalsTable <- mutate(NewGoalsTable, goals = as.character(goals))
# put in 1 category called "4 or more"
NewGoalsTable[5,"goals"] <- "4 or more" 

# glimpse at table
mykable(NewGoalsTable)

# get poisson probabilities for each category
MeanGoals
PoisProb <- dpois(c(0:3), MeanGoals)   # density function
PoisProb[5] <- 1 - ppois(3, MeanGoals) # 1 - cummulative prob
PoisProb <- round(PoisProb, digits = 3)
mykable(PoisProb) 

sum(PoisProb) #quick check to make sure the prob's add up to 1

# calculate the expected number of matches for each goal category.
NewGoalsTable <- cbind(NewGoalsTable, PoisProb) 
NewGoalsTable <- mutate(NewGoalsTable, 
                        ExpectedMatches = round(numMatches * PoisProb))
NewGoalsTable %>% 
  mykable()

# bar graph to compare expected and actual Matches
NewGoalsTable %>% 
  gather(ActualMatches, ExpectedMatches, 
         key = "Type", value = "numMatches") %>% 
  ggplot(aes(x = goals, y = numMatches, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge")

# Chi-sq goodness-of-fit test
MUChisq <- chisq.test(NewGoalsTable$ActualMatches, 
                      p = NewGoalsTable$PoisProb, rescale.p = TRUE)
MUChisq

# p-value = 0.984 

# function to repeat the same process for every team
PoissonFit <- function(Team){
  TeamHome <- epl.fulldata %>% 
    filter(HomeTeam == Team) %>% 
    select(Home.Goals) %>% 
    mutate(type = "Home") %>% 
    rename(goals = Home.Goals)
  
  TeamAway <- epl.fulldata %>% 
    filter(AwayTeam == Team) %>% 
    select(Away.Goals) %>% 
    mutate(type = "Away") %>% 
    rename(goals = Away.Goals)
  
  TeamGoals <- full_join(TeamHome, TeamAway, by = c("goals","type"))
  MeanGoals <- fav_stats(TeamGoals$goals)[[6]]
  numMatches <- fav_stats(TeamGoals$goals)[[8]]
  
  GoalsTable <- TeamGoals %>% 
    group_by(goals) %>% 
    summarise(ActualMatches = n())
  
  NewGoalsTable <- GoalsTable[1:4,]
  NewGoalsTable[5,] <- sum(GoalsTable[5:nrow(GoalsTable),2])
  NewGoalsTable <- mutate(NewGoalsTable, goals = as.character(goals))
  NewGoalsTable[5,"goals"] <- "4 or more"
  
  PoisProb <- dpois(c(0:3), MeanGoals)
  PoisProb[5] <- 1 - ppois(3, MeanGoals)
  PoisProb <- round(PoisProb, digits = 3)
  ExpectedMatches <- as.integer(numMatches * PoisProb)
  NewGoalsTable <- cbind(NewGoalsTable, PoisProb, ExpectedMatches)
}

# try it for Tottenham, Liverpool
TotGoalsTable <- PoissonFit("Tottenham")
mykable(TotGoalsTable)
TotChisq <- chisq.test(TotGoalsTable$ActualMatches, 
                       p = TotGoalsTable$PoisProb, rescale.p = TRUE)
TotChisq

LivGoalsTable <- PoissonFit("Liverpool")
mykable(LivGoalsTable)
LivChisq <- chisq.test(LivGoalsTable$ActualMatches, 
                       p = LivGoalsTable$PoisProb, rescale.p = TRUE)
LivChisq

#__________________________________________________
# Exponential and time between goals

# reads in scoring time data
muscoringtime <- read_excel("data/muscoringtime.xlsx")
muscoringtime %>% 
  head(4) %>% 
  mykable()

# histogram of time between goals
muscoringtime %>%
  filter(!is.na(TimeBetween)) %>% 
  ggplot() +
  geom_histogram(mapping = aes(TimeBetween), color = "darkgreen", 
                 fill = "lightgreen", breaks = seq(1,300, by = 20))

# summarized stats of time betwee goals
mykable(fav_stats(muscoringtime$TimeBetween))

# get mean and sd, compare 1/mean and 1/sd
MeanTimeBetween <- fav_stats(muscoringtime$TimeBetween)[[6]]
1/MeanTimeBetween
StDevTimeBetween <- fav_stats(muscoringtime$TimeBetween)[[7]] 
1/StDevTimeBetween

# Kolmogorov-Smirnov goodness-of-fit test
TimeBetweenKS <- ks.test(muscoringtime$TimeBetween, 
                         "pexp", 1/MeanTimeBetween)
TimeBetweenKS

# KS test plot
x <- muscoringtime$TimeBetween
plot(ecdf(x), xlab = "Time Between", 
     ylab = "Cumulative Distribution", main = "", pch = 20)
curve((1 - exp(-(1/MeanTimeBetween)*x)), 0, 240, add = TRUE, col = "blue")

# D = 0.089 and p-value = 0.679

#__________________________________________________
# Uniform and time of goals

# re-scaling the goal minutes into an interval [0,1]
MUTime <- muscoringtime %>%
  filter(!is.na(Min)) %>% 
  mutate(StdMin = Min/(90 + H1_stoppage + H2_stoppage))

# histogram of standardize time of goals
MUTime %>% 
  ggplot() +
  geom_histogram(mapping = aes(StdMin), color = "darkgreen", 
                 fill = "lightgreen", breaks = seq(0, 1, by=0.1))

# numerical summary
fav_stats(round(MUTime$StdMin, 3)) %>% 
  mykable()

# get mean and variance, compare to theoretical mean 1/2 and variance 1/12
TotalN <- fav_stats(MUTime$StdMin)[[8]]
MeanStdTime <- fav_stats(MUTime$StdMin)[[6]]
MeanStdTime
1/2
VarianceStdTime <- fav_stats(MUTime$StdMin)[[7]]^2
VarianceStdTime
1/12

# KS gof test
TimeUnifKS <- ks.test(MUTime$StdMin, "punif", 0, 1)
TimeUnifKS

# KS test plot
ks.plot.unif(MUTime$StdMin, xlab = "StdTime", col.conf = "white")
# D = 0.0854, p-value = 0.73

#__________________________________________________
#__________________________________________________
# Predicting 2018-19 Season Results From Prior Data

# filter out 2018-19 data
epl.data <- epl.fulldata %>% 
  filter(Season != "2018-19")

# grab the team names
Teams <- as.data.frame(unique(epl.data$HomeTeam))
colnames(Teams) <- c("TeamName")
Teams <- arrange(Teams, TeamName)

# poisson regression model for home teams
HomeReg <- glm(Home.Goals ~ HomeTeam, 
               family = poisson(link = "log"),
               data = epl.data)

# get the coefficients from the model
HomeTable <- as.data.frame(coefficients(HomeReg))
names(HomeTable)[1] <- "Coeff" 
HomeIntercept <- HomeTable[1,1] # get the model's y-intercept
HomeTable[1,1] <- 0 # reference group
HomeTable[,2] <- Teams$TeamName # put the team names into table
names(HomeTable)[2] <- "HomeTeam"

# calculate expected home scoring rate for every team
HomeTable <- HomeTable %>% 
  mutate(HomeRate = round(exp(HomeIntercept + Coeff), 3), # back-transform
         FakeCol = "fake") # fake column for joining purpose
HomeTable[,1:3] %>% 
  head(4) %>% 
  mykable()

# repeat the same process for away teams
AwayReg <- glm(Away.Goals ~ AwayTeam, 
            family = poisson(link = "log"),
            data = epl.data) 
AwayTable <- as.data.frame(coefficients(AwayReg))
names(AwayTable)[1] <- "Coeff"
AwayIntercept <- AwayTable[1,1]
AwayTable[1,1] <- 0
AwayTable[,2] <- Teams$TeamName
names(AwayTable)[2] <- "AwayTeam"
AwayTable <- AwayTable %>% 
  mutate(AwayRate = round(exp(AwayIntercept + Coeff), 3),
         FakeCol = "fake") 

# join home and away rate tables
FullTable <- full_join(HomeTable, AwayTable, by = "FakeCol") 
FullTable <- FullTable %>% 
  filter(HomeTeam != AwayTeam) %>% 
  select(HomeTeam, HomeRate, AwayTeam, AwayRate)

# get all 2018-19 matchups
Teams1819 <- epl.fulldata %>%   # get the 18-19 teams 
  filter(Season == "2018-2019") %>% 
  select(HomeTeam)
Teams1819 <- unique(Teams1819)
Table1819 <- FullTable %>%   # only keep 18-19 teams
  filter(HomeTeam %in% Teams1819$HomeTeam,  
         AwayTeam %in% Teams1819$HomeTeam)

nrow(Table1819) # 380 total matches
Table1819 %>% 
  head(3) %>% 
  mykable()

### Simulation
nSim <- 10000  # duplicate the 2018-19 table 10000 times
SimTable <- Table1819 %>% 
  slice(rep(row_number(), nSim)) # rep(): replicate the rows 
                                 # slice(): choose rows 

# generate number of goals for every team (matchup, simulation)
SimTable <- SimTable %>% 
  mutate(HomeScore = rpois(nrow(SimTable), HomeRate),
         AwayScore = rpois(nrow(SimTable), AwayRate),
         HomePoints = ifelse(HomeScore > AwayScore, 3, 
                             ifelse(HomeScore == AwayScore, 1, 0)), 
         AwayPoints = ifelse(HomeScore > AwayScore, 0,
                             ifelse(HomeScore == AwayScore, 1, 3)))

# every 380-row segment contains the result of 1 simulated season
# write function to get each individual simulation 
# and also calculate points, goal differentials and get the team ranks for each sim season.
Sim <- function(simNum){
  firstRow <- 380*simNum - 379 # first row of each season 
  lastRow <- 380*simNum        # last row
  MyTable <- SimTable[firstRow:lastRow,] # get each season's table
  
  Home1819 <- MyTable %>%      # get home results
    group_by(HomeTeam) %>% 
    summarise(TotalHomePoints = sum(HomePoints), # points
              TotalHomeScored = sum(HomeScore),  # goals scored
              TotalHomeConceded = sum(AwayScore)) %>% # goals conceded
    rename(Team = HomeTeam)
  
  Away1819 <- MyTable %>%      # get away results
    group_by(AwayTeam) %>% 
    summarise(TotalAwayPoints = sum(AwayPoints),
              TotalAwayScored = sum(AwayScore),  
              TotalAwayConceded = sum(HomeScore)) %>%  
    rename(Team = AwayTeam)
  
  # join home and away tables
  PointsTable <- full_join(Home1819, Away1819)
  
  # calculate total points and GD (= goals scored - conceded) 
  PointsTable <- PointsTable %>% 
    mutate(FinalPoints = TotalHomePoints + TotalAwayPoints, 
           GD = TotalHomeScored - TotalHomeConceded +       
                TotalAwayScored - TotalAwayConceded) %>% 
    arrange(desc(FinalPoints), desc(GD)) %>% 
    mutate(SimNum = simNum, # distinguish the sim's
           Rank = 1:20) %>% # rank for each team
    select(Rank, Team, FinalPoints, GD, SimNum) 
}

# 2 sample simulated seasons
mykable(Sim(1)) # "1st sim season"
mykable(Sim(1000)) # "1000th sim season"

# get results of each one of our 10000 simulations 
# put everything together into a data frame 

EPLSim_All <- Sim(1) # initialize
for (i in 2:nSim) { # for loop to combine all sim seasons
  EPLSim_All <- EPLSim_All %>% 
    full_join(Sim(i))
}

### Analysis

# write sim results to a csv file in the "sim_results" folder
write_excel_csv(EPLSim_All, "sim_results/EPLSimFull.csv")

# read in to analyze
EPLSim_All <- read.csv("sim_results/EPLSimFull.csv")
EPLSim_All <- EPLSim_All %>% 
  mutate(SimType = "All Seasons")

# team's chances of finishing at each position
AllRankTable <- table(EPLSim_All$Team, EPLSim_All$Rank)/nSim
AllRankTable %>% 
  mykable() %>% 

# first place finish
EPLSim_All %>% 
  filter(Rank == 1) %>% 
  group_by(Team) %>% 
  summarise(Pct = 100*n()/(nSim)) %>% 
  arrange(desc(Pct)) %>% 
  head(6) %>% 
  mykable()

# top 4 finish
EPLSim_All %>% 
  filter(Rank %in% c(1,2,3,4)) %>% 
  group_by(Team) %>%
  summarise(Pct = 100*n()/(nSim)) %>%
  arrange(desc(Pct)) %>% 
  head(6) %>% 
  mykable()

# big 6 teams
EPLBig6 <- EPLSim_All %>% 
  filter(Team %in% c("Man United", "Liverpool", "Arsenal", 
                     "Chelsea", "Tottenham", "Man City"))

# side by side boxplots for rank and final points
EPLBig6 %>% 
  ggplot(mapping = aes(x = Team, y = Rank)) +
  geom_boxplot(color = "brown")
EPLBig6 %>% 
  ggplot(mapping = aes(x = Team, y = FinalPoints)) +
  geom_boxplot(color = "brown")

# scatterplot illustrating points and rank relationship
EPLSim_All %>%
  group_by(Team) %>% 
  summarise(meanPts = mean(FinalPoints),
            meanRank = mean(Rank)) %>% 
  ggplot(mapping = aes(meanRank, meanPts)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# relegation zone
EPLSim_All %>% 
  filter(Rank %in% c(18, 19, 20)) %>% 
  group_by(Team) %>%
  summarise(Pct = 100*n()/(nSim)) %>%
  arrange(desc(Pct)) %>% 
  head(4) %>% 
  mykable()

# 40pt safety rule: teams need 40 to be safe from relegation
EPLSim_All %>% 
  filter(Rank %in% c(18,19,20) & FinalPoints >= 40) %>% 
  summarise(numSimSeasons = n_distinct(SimNum), numTeams = n()) %>% 
  mykable()

EPLSim_All %>% 
  filter(Rank %in% c(18,19,20)) %>% 
  summarise(AvgFinalPoints = mean(FinalPoints)) %>% 
  mykable()

## Using Data From the 2010s Only to predict

# same process as previous approach
# Done in a separate script, can be found in sim_results folder
EPLSim_2010s <- read.csv("sim_results/EPLSim2010s.csv")
EPLSim_2010s <- EPLSim_2010s %>% 
  mutate(SimType = "2010s")

## Using All the Data but Assign More Weight to Recent Seasons

# assign weight
Weights <- tribble( ~Season,  ~Weight,
                    "1992-93", 1,
                    "1993-94", 1,
                    "...", 1,
                    "2011-12", 1,
                    "2012-13", 1,
                    "2013-14", 2,
                    "2014-15", 3,
                    "2015-16", 4,
                    "2016-17", 8,
                    "2017-18", 16)

Weights$Season <- factor( Weights$Season, 
         levels = c("1992-93", "1993-94", "...",
                    "2011-12", "2012-13", "2013-14",
                    "2014-15", "2015-16", "2016-17", "2017-18"))
Weights %>% 
  ggplot(mapping = aes(x = Season, y = Weight, group = 1)) +
  geom_point() +
  geom_line()

# create table with different weights for each season. 
epl9213 <- epl.fulldata[1:8226,] # data from 1992-93 to 2012-13
# get data from epl.season function
# duplicate each season based on its weight
epl1314 <- epl.season(2013) 
epl1314 <- epl1314 %>% slice(rep(row_number(), 2)) 
epl1415 <- epl.season(2014) 
epl1415 <- epl1415 %>% slice(rep(row_number(), 3)) 
epl1516 <- epl.season(2015) 
epl1516 <- epl1516 %>% slice(rep(row_number(), 4)) 
epl1617 <- epl.season(2016) 
epl1617 <- epl1617 %>% slice(rep(row_number(), 8)) 
epl1718 <- epl.season(2017) 
epl1718 <- epl1718 %>% slice(rep(row_number(), 16)) 
eplWt <- epl9213 %>% # join the tables
  full_join(epl1314) %>% 
  full_join(epl1415) %>% 
  full_join(epl1516) %>%
  full_join(epl1617) %>%
  full_join(epl1718) 

# same process as previous two approaches, write to a csv file
EPLSim_Wt <- read.csv("sim_results/EPLSimWt.csv")
EPLSim_Wt <- EPLSim_Wt %>% 
  mutate(SimType = "Assign Weight")

### Comparison

# combine 3 types of sim into a table to compare
SimComparison <- EPLSim_All %>% 
  full_join(EPLSim_2010s) %>% 
  full_join(EPLSim_Wt) 

# boxplots comparing the Big 6's rank and final points 

SimComparison$SimType <- 
  factor(SimComparison$SimType, 
         levels = c("All Seasons", "2010s", "Assign Weight"))
SimComparison %>%
  filter(Team %in% EPLBig6$Team) %>% 
  ggplot(mapping = aes(SimType, Rank)) +
  geom_boxplot() +
  facet_wrap(~ Team)
SimComparison %>%
  filter(Team %in% EPLBig6$Team) %>% 
  ggplot(mapping = aes(SimType, FinalPoints)) +
  geom_boxplot() +
  facet_wrap(~ Team)

# compare big 6 ranks
Big6Rank <- SimComparison %>% 
  filter(Team %in% EPLBig6$Team) %>% 
  filter(Rank == 1) %>% 
  group_by(Team, SimType) %>% 
  summarise(Pct = 100*n()/(nSim))
Big6Rank %>% 
  spread(SimType, Pct) %>% 
  mykable()

# relegation zone
SimComparison %>% 
  filter(Rank %in% c(18, 19, 20)) %>% 
  group_by(Team, SimType) %>% 
  summarise(Pct = 100*n()/(nSim)) %>% 
  spread(SimType, Pct) %>% 
  arrange(desc(`2010s`)) %>% 
  mykable()

# 40pt safety rule
SimComparison %>% 
  filter(Rank %in% c(18,19,20) & FinalPoints >= 40) %>% 
  group_by(SimType) %>% 
  summarise(numSimSeasons = n_distinct(SimNum), numTeams = n()) %>% 
  mykable() 
