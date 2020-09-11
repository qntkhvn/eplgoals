## Simulate by putting more weights towards recent seasons
## Last 5 years: all that matters
# last5 <- c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18")

### weight scheme
### from 1992-93 to 2012-13: 1 
### 13-14: 2; 14-15: 3; 15-16: 4; 16-17:8; 17-18:16

epl9213 <- epl.fulldata[1:8226,] # data from 1992-93 to 2012-13

epl1314 <- epl.season(2013)  # 2013-14
epl1314 <- epl1314 %>% slice(rep(row_number(), 2))

epl1415 <- epl.season(2014)  # 2014-15
epl1415 <- epl1415 %>% slice(rep(row_number(), 3))

epl1516 <- epl.season(2015) # 2015-16
epl1516 <- epl1516 %>% slice(rep(row_number(), 4))

epl1617 <- epl.season(2016) # 2016-17
epl1617 <- epl1617 %>% slice(rep(row_number(), 8))

epl1718 <- epl.season(2017) # 2017-18
epl1718 <- epl1718 %>% slice(rep(row_number(), 16))

# combining seasons
eplWt <- epl9213 %>% 
  full_join(epl1314) %>% 
  full_join(epl1415) %>% 
  full_join(epl1516) %>%
  full_join(epl1617) %>%
  full_join(epl1718) 

TeamsWt <- as.data.frame(unique(epl.fulldata$HomeTeam))
colnames(TeamsWt) <- c("TeamName")
TeamsWt <- TeamsWt %>% arrange(TeamName)

# Home
Home_Weighted <- glm(Home.Goals ~ HomeTeam, 
            family = poisson(link = "log"),
            data = eplWt)
HomeTable <- as.data.frame(coefficients(Home_Weighted))
names(HomeTable)[1] <- "Coeff"
HomeIntercept <- HomeTable[1,1]
HomeTable[1,1] <- 0
HomeTable[,2] <- TeamsWt$TeamName
names(HomeTable)[2] <- "HomeTeam"
HomeTable <- HomeTable %>% 
  mutate(HomeRate = exp(HomeIntercept + Coeff),
         FakeCol = "fake")

# Away
Away_Weighted <- glm(Away.Goals ~ AwayTeam, 
            family = poisson(link = "log"),
            data = eplWt) 
AwayTable <- as.data.frame(coefficients(Away_Weighted))
names(AwayTable)[1] <- "Coeff"
AwayIntercept <- AwayTable[1,1]
AwayTable[1,1] <- 0
AwayTable[,2] <- TeamsWt$TeamName
names(AwayTable)[2] <- "AwayTeam"
AwayTable <- AwayTable %>% 
  mutate(AwayRate = exp(AwayIntercept + Coeff),
         FakeCol = "fake")

# Joining tables
FullTable <- full_join(HomeTable, AwayTable, by = "FakeCol") 
FullTable <- FullTable %>% 
  filter(HomeTeam != AwayTeam) %>% 
  select(HomeTeam, HomeRate, AwayTeam, AwayRate)

Teams1819 <- epl.fulldata %>% filter(Season == "2018-2019") %>% select(HomeTeam)
Teams1819 <- unique(Teams1819)
Table1819 <- FullTable %>% 
  filter(HomeTeam %in% Teams1819$HomeTeam,
         AwayTeam %in% Teams1819$HomeTeam) 

SimTable <- Table1819 %>% 
  slice(rep(row_number(), 10000))
SimTable <- SimTable %>% 
  mutate(HomeScore = rpois(nrow(SimTable), HomeRate),
         AwayScore = rpois(nrow(SimTable), AwayRate),
         HomePoints = ifelse(HomeScore > AwayScore, "3", 
                             ifelse(HomeScore == AwayScore, "1", "0")),
         AwayPoints = ifelse(HomeScore > AwayScore, "0",
                             ifelse(HomeScore == AwayScore, "1", "3")),
         HomePoints = as.integer(HomePoints),
         AwayPoints = as.integer(AwayPoints),
         HomeResult = ifelse(HomeScore > AwayScore, "W", 
                             ifelse(HomeScore == AwayScore, "D", "L")))

nSim <- 10000
EPLSim_Wt <- Sim(1)
for (i in 2:nSim) {
  EPLSim_Wt <- EPLSim_Wt %>%
    full_join(Sim(i))
}

write_excel_csv(EPLSim_Wt, "EPLSimWt.csv") # write sim results to a csv file
#EPLSim_Wt <- read.csv("C:/Users/nminh/Desktop/USB copy/EPLSimWt.csv")
