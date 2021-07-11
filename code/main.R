#  main.R
#  Created: 7/10/21
#  Author:  JStromme
#
#  Who plays most randomly in the NBA? A simple statistical analysis
#  (see tex doc for logic)
# 

# Preliminaries ----------------------------------------------------------

rm(list = ls()) # clear workspace
setwd("/Users/John/Documents/Wisconsin/Research/nba/nba-randomness/")

library(tidyverse)
library(tidylog); options("tidylog.display" = list(print))
library(lubridate)

# Read data ------------------------------------------

df19 <- read_csv('./data/NBA_PBP_2019-20_truncated.csv') %>% 
#df19 <- read_csv('./data/NBA_PBP_2019-20.csv') %>% 
  filter(GameType == 'regular') %>% 
  mutate(Date = mdy(Date)) %>% 
  select(URL, Date, Quarter, SecLeft, AwayTeam, AwayPlay, HomeTeam, HomePlay, 
         ShotType, ShotOutcome, ShotDist, FreeThrowOutcome, FreeThrowNum,
         TurnoverType,HomeScore, AwayScore, ReboundType, JumpballPoss,
         FoulType, EnterGame, TimeoutTeam,WinningTeam) %>% 
  rename(gameid = URL)
df20 <- read_csv('./data/NBA_PBP_2020-21.csv') %>% 
  filter(GameType == 'regular') %>% 
  mutate(Date = mdy(Date)) %>% 
  select(URL, Date, Quarter, SecLeft, AwayTeam, AwayPlay, HomeTeam, HomePlay, 
       ShotType, ShotOutcome, ShotDist, FreeThrowOutcome, FreeThrowNum,
       TurnoverType, HomeScore, AwayScore, ReboundType, JumpballPoss,
       FoulType, EnterGame, TimeoutTeam,WinningTeam) %>% 
  rename(gameid = URL) 

#combine datasets together 
df <- rbind(df19, df20)
# subset 19 so we have one 'season'
cutoffdate = max(df20$Date)-365
df <- df %>%
  filter(Date > cutoffdate) %>% 
  select(-Date)
rm(df19, df20,cutoffdate)

#create a dataframe of team wins over sample period
teamwins <- df %>% 
  group_by(gameid, WinningTeam) %>% 
  select(gameid, WinningTeam) %>% 
  distinct() %>% 
  group_by(WinningTeam) %>% 
  mutate(countme = 1) %>% 
  summarise(wins = sum(countme)) %>% 
  rename(team = WinningTeam)
df <- df %>% select(-WinningTeam)

#identify and remove 'garbage time'
df <- df %>% 
  mutate(spread = abs(HomeScore - AwayScore)) %>% 
  mutate(garbagetime = if_else( Quarter==4 & (spread > 20 |
                                                (spread > 15 & SecLeft < 120) |
                                                (spread > 10 & SecLeft < 60)),
                                1, 0)) %>% 
  filter(garbagetime == 0) %>% 
  select(-spread, -garbagetime, -HomeScore, -AwayScore)

# First as a simpler project, just do shot distance gini ------------

#save copy of df so can do other stuff with it later
dfcopy <- df

# only care about shots:
df <- df %>% 
  filter(!is.na(ShotDist))

# need to get a dataset with columns: TEAM, SHOT DISTANCE
#first label each shot as home or away
df <- df %>% 
  mutate(ShotTeam = if_else(!is.na(AwayPlay), AwayTeam, HomeTeam))
#get rid of things we don't need
df <- df %>% 
  select(ShotTeam, ShotDist)
#truncate long threes into a 'deep3' category, 27-40 feet. drop the prayer shots
df <- df %>% 
  filter(ShotDist <= 40) %>% 
  mutate(ShotDist = if_else(ShotDist > 26, 27, ShotDist))
#roll up into frequencies for each team-distance
df <- df %>% 
  mutate(counter = 1) %>% 
  group_by(ShotTeam, ShotDist) %>% 
  summarise(ShotCount = sum(counter))

#Now by team we can compute a Gini!

#loop over teams, for testing we start with win
teamvec <- unique(df$ShotTeam)

#empty df
randomgini <- df %>% 
  group_by(ShotTeam) %>% 
  summarise(shotgini = sum(ShotCount)) %>% 
  mutate(shotgini = NA) %>% 
  rename(team = ShotTeam)

for (team in teamvec) {
  dfsub <- df %>% filter(ShotTeam == team)
  #create an absolute 'difference matrix' for Gini numerator
  diffmat <- abs(outer(dfsub$ShotCount, dfsub$ShotCount, '-'))
  numerator <- sum(diffmat)
  #and the denominator calcs:
  xbar <- sum(dfsub$ShotCount) / nrow(dfsub)
  denominator <- 2 * nrow(dfsub)^2 * xbar
  gini = numerator / denominator
  randomgini$shotgini[randomgini$team==team] <- gini
}
rm(team, dfsub, diffmat, numerator, xbar, denominator, gini)

#make gini easier to read
randomgini$shotgini <- round(randomgini$shotgini * 100, 1)


# Now work on shot clock gini -------------------------
df <- dfcopy 
#rm(dfcopy)

#first, most difficult, is we need to impute what the shot clock was at each shot
#possibilities that can start the shot clock with a fresh *24* (ignoring 14):
# - Rebound by *other* team (after shot attempt or missed FT)
# - Made shot
# - Turnover

#create variable for which team had action (and cut superfluous vars):
df <- df %>% 
  mutate(team = if_else(!is.na(AwayPlay), AwayTeam, HomeTeam),
         otherteam = if_else(is.na(AwayPlay), AwayTeam, HomeTeam)) %>% 
  mutate(description = coalesce(HomePlay, AwayPlay)) %>% 
  select(-AwayTeam, -HomeTeam, -AwayPlay,-HomePlay)
#ignore first FTs and technicals which could never result in change in posession
df <- df %>% 
  filter(!(FreeThrowNum %in% c('1 of 2', '1 of 3', '2 of 3','technical'))) %>% 
  select(-FreeThrowNum)
#offensive rebounds can't result in change of poss
df <- df %>% 
  filter((ReboundType=='defensive') |is.na(ReboundType))
#ignore fouls, substitutions and timeouts (no poss change here)
df <- df %>% 
  filter(is.na(FoulType)) %>%
  filter(is.na(EnterGame)) %>% 
  filter(is.na(TimeoutTeam)) %>% 
  select(-FoulType, -EnterGame, -TimeoutTeam)
# don't care about end of quarter flags, ejections, replays, kicked ball, 
#def 3 sec, delay of game, def goaltend. ignoring rare rare double lane and jump violations
#dropping random typo jump ball vs.
df <- df %>% 
  filter(!str_starts(description,"End of")) %>% 
  filter(!str_detect(description,"ejected")) %>% 
  filter(!str_detect(description,"Replay")) %>% 
  filter(!str_detect(description,"kicked")) %>% 
  filter(!str_detect(description,"Def 3 sec tech foul by Team")) %>% 
  filter(!str_detect(description,"delay of game")) %>% 
  filter(!str_detect(description,"double lane")) %>% 
  filter(!str_detect(description,"Violation by Team \\(jump ball\\)")) %>% 
  filter(!str_detect(description,"Jump ball:  vs.")) %>% 
  filter(!str_detect(description,"Violation by M. Williams")) %>% 
  filter(!str_detect(description,"def goaltend"))
# check that we aren't missing any explanations:
#df$test <- !is.na(df$ShotType) | !is.na(df$ShotOutcome) | !is.na(df$ShotDist) |
#  !is.na(df$FreeThrowOutcome) | !is.na(df$TurnoverType) | !is.na(df$ReboundType)|
#  !is.na(df$JumpballPoss)
#sum(df$test==0)
#df %>% filter(test==F) %>% View()

#create var for when shot clock reset
#The shot clock shall be reset to 24 seconds anytime the following occurs:
#-Change of possession from one team to another
#-Personal foul where ball is being inbounded in backcourt
#-Violation where ball is being inbounded in backcourt
#-Jump balls which are not the result of a held ball caused by the defense
#-All flagrant and punching fouls
#first define special possesion changes var we will use later to know jump ball poss
df <- df %>% 
  mutate(posschange = 0) %>% 
  mutate(posschange = if_else(ShotOutcome == 'make' & !is.na(ShotOutcome),
                                  1,posschange)) %>% #change poss after made basket
  mutate(posschange = if_else(FreeThrowOutcome == 'make' & !is.na(FreeThrowOutcome),
                                  1,posschange)) %>% #change poss after made FT
  mutate(posschange = if_else(!is.na(TurnoverType),
                                  1,posschange)) #change poss on turnover
  
df <- df %>% 
  mutate(shotclockreset = 0) %>% 
  mutate(shotclockreset = if_else(SecLeft == 720,
                                 1,shotclockreset)) %>% #start of quarter
  mutate(shotclockreset = if_else((SecLeft==300 & Quarter>4),
                                  1,shotclockreset)) %>% #start of OT periods
  group_by(gameid, Quarter) %>% 
  mutate(shotclockreset = if_else(row_number()==1 & !is.na(JumpballPoss),
                                  1,shotclockreset)) %>% 
  ungroup() %>% 
  mutate(shotclockreset = if_else(posschange==1, 
                                  1,shotclockreset)) %>% #possesion change
  mutate(shotclockreset = if_else(ReboundType == 'defensive' & !is.na(ReboundType),
                              1,shotclockreset)) %>% #change poss after rebound
  mutate(shotclockreset = if_else(str_detect(description,'flagrant'),
                                  1,shotclockreset)) #reset after flagrant
  
  
#jump balls mid-game need to have special logic, reset shot clock if posession changes after jump
#(there are cases of reset to 24 after jump, i.e. out of bounds tie, but too rare and I can't observe those
#  so need to make an assumption only reset on poss change. it shouldn't matter much)
df <- df %>% 
  group_by(gameid, Quarter) %>% 
  mutate(posession = "idk") %>% 
  mutate(posession = if_else(lag(shotclockreset,1)==1,
                              lag(team),posession)) %>% 
  mutate(posession = if_else(lag(posschange,1)==1,
                             lag(otherteam),posession)) %>% 
  mutate(posession = if_else(lag(ShotOutcome,1)=='miss' & !is.na(lag(ShotOutcome,1)),
                             lag(team),posession)) %>% 
  mutate(posession = if_else(lag(FreeThrowOutcome,1)=='miss' & !is.na(lag(FreeThrowOutcome,1)),
                             lag(team),posession)) %>% 
  mutate(shotclockreset = if_else(team!=posession & !is.na(JumpballPoss) & row_number() != 1, 
                                  1,shotclockreset)) %>% #if winner of jump ne prev posessor, reset shot clock
  ungroup()
#jump logic: shotclockreset from a non-posschange, take team listed under shot clock reset as having posession before jump
#  posschange, take other team as having possession before jump (will overwrite accordingly so leave order in code)

# some notes:
# -dont need to worry about and-1s being a 'shotclockreset' on both shot and FT. doesn't 
#matter for our logic: the reset after the and-1 scrubs away the erroneous 'shotclockreset' on shot attempt
# - 

# now we create shot clock stuff----
df <- df %>% 
  group_by(gameid, Quarter) %>% 
  mutate(possid = cumsum(shotclockreset)) %>% 
  mutate(possid = lag(possid,1)) %>% 
  mutate(timedelta = (SecLeft - lag(SecLeft,1))) %>% 
  mutate(shotclock_new = if_else(shotclockreset==1,24,NA_real_)) %>% 
  group_by(gameid, Quarter, possid) %>% 
  mutate(shotclock = 24 + cumsum(timedelta))

# subset to only include first shot attempt per posession
df <- df %>% 
  group_by(gameid, Quarter, possid) %>% 
  mutate(shotcount = cumsum(!is.na(ShotType))) %>% 
  filter(shotcount == 1)

# roll up by shot clock
# note: data is pretty messy, sometimes game log has missing stuff, sometimes
# doesn't scrape correctly? so shotclock imputation is noisy, but seems to 
# *mostly* look good.
df <- df %>% 
  filter(shotclock >=0 & shotclock <=19) %>% 
  group_by(team, shotclock) %>% 
  summarise(ShotCount = sum(!is.na(ShotType))) %>% 
  rename(ShotTeam = team)
#create gini
randomgini$clockgini <- NA
for (team in teamvec) {
  dfsub <- df %>% filter(ShotTeam == team)
  #create an absolute 'difference matrix' for Gini numerator
  diffmat <- abs(outer(dfsub$ShotCount, dfsub$ShotCount, '-'))
  numerator <- sum(diffmat)
  #and the denominator calcs:
  xbar <- sum(dfsub$ShotCount) / nrow(dfsub)
  denominator <- 2 * nrow(dfsub)^2 * xbar
  gini = numerator / denominator
  randomgini$clockgini[randomgini$team==team] <- gini
}
rm(team, dfsub, diffmat, numerator, xbar, denominator, gini)

#make gini easier to read
randomgini$clockgini <- round(randomgini$clockgini * 100, 1)


## PLOT
library(ggimage)
library(slickR)
randomgini$logoteam <- nba_team_logo$team
randomgini$logo <- nba_team_logo$uri
#alphabetized lines up except swap charlotte and chicago 
randomgini[4,4:5] <- nba_team_logo[5,]
randomgini[5,4:5] <- nba_team_logo[4,]
ggplot(randomgini, aes(x=shotgini,y=clockgini)) + 
  geom_image(aes(image=logo),size=.1) +
  theme_void() +
  xlim(33,60) +
  ylim(12,24) +
  geom_hline(yintercept = 18) +
  geom_vline(xintercept = 46.5) #+ 
  #ggtitle("Which Teams Play More Randomly?") +
  #theme(plot.title = element_text(hjust = 0.5))
ggsave('./plots_tables/randomgini.png',device = 'png', height = 5, width = 5)
  
## Correlation between randomness and wins?
randomgini <- left_join(randomgini,teamwins)
cor(randomgini$shotgini, randomgini$wins) #-0.1822751
cor(randomgini$clockgini, randomgini$wins) #-0.2352987

## Export table with full gini data
exporttable <- randomgini %>% select(team, shotgini, clockgini)
#add rankings
exporttable <- exporttable %>% 
  arrange(-shotgini) %>% 
  mutate(distancerank = row_number()) %>% 
  arrange(-clockgini) %>% 
  mutate(timerank = row_number()) %>% 
  arrange(team) %>% 
  rename(Team = team)

library(stargazer)
starout <- stargazer(exporttable, summary=F, rownames = F, 
                     title = "Inverse Gini Coefficients for All Teams")
starout <- gsub(x=starout, pattern='shotgini',
     replacement='Distance i-Gini', fixed=T)
starout <- gsub(x=starout, pattern='clockgini',
                replacement = 'Time i-Gini', fixed = T)
starout <- gsub(x=starout, pattern='distancerank',
                replacement = 'Distance Rank', fixed = T)
starout <- gsub(x=starout, pattern='timerank',
                replacement = 'Time Rank', fixed = T)
cat(starout,sep='\n',file='./plots_tables/ginitable.tex')

