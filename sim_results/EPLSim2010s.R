### Using only data from seasons in the 2010s

# get 2010s data
epl.2010s <- epl.results %>% 
  filter(Season %in% c("2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13", "2011-12", "2010-11"))

Teams <- as.data.frame(unique(epl.2010s$HomeTeam))
colnames(Teams) <- c("TeamName")
Teams <- Teams %>% arrange(TeamName)

# Poisson Regression for HomeTeam
Home <- glm(Home.Goals ~ HomeTeam, 
            family = poisson(link = "log"),
            data = epl.2010s)
# Get the coefficients and then back transform to get the actual rate
HomeTable <- as.data.frame(coefficients(Home))
names(HomeTable)[1] <- "Coeff"
HomeIntercept <- HomeTable[1,1]
HomeTable[1,1] <- 0
HomeTable[,2] <- Teams$TeamName
names(HomeTable)[2] <- "HomeTeam"
HomeTable <- HomeTable %>% 
  mutate(HomeRate = exp(HomeIntercept + Coeff),
         FakeCol = "fake")

# Poisson Regression for AwayTeam
Away <- glm(Away.Goals ~ AwayTeam, 
            family = poisson(link = "log"),
            data = epl.2010s) 
# Get the coefficients and then back transform to get the actual rate
AwayTable <- as.data.frame(coefficients(Away))
names(AwayTable)[1] <- "Coeff"
AwayIntercept <- AwayTable[1,1]
AwayTable[1,1] <- 0
AwayTable[,2] <- Teams$TeamName
names(AwayTable)[2] <- "AwayTeam"
AwayTable <- AwayTable %>% 
  mutate(AwayRate = exp(AwayIntercept + Coeff),
         FakeCol = "fake")

# Joining tables
FullTable <- full_join(HomeTable, AwayTable, by = "FakeCol") 
FullTable <- FullTable %>% 
  filter(HomeTeam != AwayTeam) %>% 
  select(HomeTeam, HomeRate, AwayTeam, AwayRate)

Teams1819 <- epl.results %>% filter(Season == "2018-19") %>% select(HomeTeam)
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
EPLSim_2010s <- Sim(1)
for (i in 2:nSim) {
  EPLSim_2010s <- EPLSim_2010s %>%
    full_join(Sim(i))
}

# write_excel_csv(EPLSim_2010s, "sim_results/EPLSim2010s.csv") # write sim results to a csv file
# EPLSim_2010s <- read_csv("sim_results/EPLSim2010s.csv")
