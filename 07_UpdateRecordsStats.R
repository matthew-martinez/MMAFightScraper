library(tidyverse)

# This is for cleaning up the scraped event records file
# Making the Method variable readable and adding in fighter age at time of fight

setwd("/home/m/Documents/Projects/Data/MMA") # change to directory you want to work/save files in
eventFile <- "/home/m/Documents/Projects/Data/MMA/Events/eventRecords_2022_combined.csv" # change to path for full records .csv

date <- Sys.Date()

d <- read.csv(eventFile)[,2:15]
d <- d %>% select(-New.ID)
d$Date <- as.Date(d$Date, "%m/%d/%Y")

d2 <- read.csv("./Events/Revent-records95784.csv")[,2:14]
d2 <- d2 %>%
 rename(Fight.Number = Match)
d2$Date <- as.Date(d2$Date, "%Y-%m-%d")

d <- rbind (d, d2)

# Making sure all Draws are coded as Draw in the Result column
d$Result[str_detect(d$Method, "Draw") == TRUE] <- "Draw"
d$Result[str_detect(d$Method, "Drew") == TRUE] <- "Draw"

# Cleaning up the Method column
# Possible making a new column for decisions - if unanimous or split, etc.
d$Method[str_detect(d$Method, "TKO") == TRUE] <- "Technical Knockout"
d$Method[str_detect(d$Method, "Tko") == TRUE] <- "Technical Knockout"
d$Method[str_detect(d$Method, "KO") == TRUE] <- "Knockout"
d$Method[str_detect(d$Method, "Ko") == TRUE] <- "Knockout"
d$Method[str_detect(d$Method, "K.O") == TRUE] <- "Knockout"
d$Method[str_detect(d$Method, "Submission") == TRUE] <- "Submission"
d$Method[str_detect(d$Method, "Decision") == TRUE] <- "Decision"
d$Method[str_detect(d$Method, "Draw") == TRUE] <- "Draw"
d$Method[str_detect(d$Method, "No Contest") == TRUE] <- "No Contest"
d$Method[str_detect(d$Method, "Disqualification") == TRUE] <- "Disqualification"
d$Method[str_detect(d$Method, "DQ") == TRUE] <- "Disqualification"
d$Method[str_detect(d$Method, "Submision") == TRUE] <- "Submission"
d$Method[str_detect(d$Method, "Submssion") == TRUE] <- "Submission"
d$Method[str_detect(d$Method, "Submisison") == TRUE] <- "Submission"

# Bringing in scraped stats for each fighter 
dStats <- read.csv("./Stats/MMA_Fighter_Stats2023-01-23.csv")[,2:5] # update to latest fighter stats file
dStatsFighter <- dStats %>% rename(FighterID = FighterId,
                            FighterBirthdate = Birthdate,
                            FighterWeight = Weight,
                            FighterHeight = Height)
dStatsFighter$FighterBirthdate <- as.Date(dStatsFighter$FighterBirthdate, "%b %d, %Y")

# Recreating stats for each opponentID
dStatsOpponent <- dStats %>% rename(OpponentID = FighterId,
                                    OpponentBirthdate = Birthdate,
                                    OpponentWeight = Weight,
                                    OpponentHeight = Height)
dStatsOpponent$OpponentBirthdate <- as.Date(dStatsOpponent$OpponentBirthdate, "%b %d, %Y")

# Merging stats for each fight - needs to be done twice, for the fighter and opponent
d <- merge(d, dStatsFighter, by="FighterID", all=FALSE)
d <- merge(d, dStatsOpponent, by="OpponentID", all=FALSE)

# Keeping on distinct rows
d <- d %>%
  distinct()

# Calculating age at time of fight - outputs in number of days, dividing and rounding to get years.
d$FighterAge <- round((d$Date - d$FighterBirthdate)/365,2)
d$OpponentAge <- round((d$Date - d$OpponentBirthdate)/365,2)

# Elo
elo <- read.csv("./Elo/scores_2023-01-22.csv")[,2:6] # update to latest elo file
elo$Date <- as.Date(elo$Date, "%Y-%m-%d")

fighterElo <- elo %>%
  filter(is.na(Date) == FALSE) %>%
  rename(FighterPreFightScore = PreFightScore,
         FighterPostFightScore = Score)

opponentElo <- elo %>%
  filter(is.na(Date) == FALSE) %>%
  rename(OpponentPreFightScore = PreFightScore,
         OpponentPostFightScore = Score)

final_file <- merge(d, fighterElo, by.x=c("FighterID", "Date", "Fight.Number"), by.y=c("Fighter","Date", "Fight.Number"))

final_file <- merge(final_file, opponentElo, by.x=c("OpponentID", "Date", "Fight.Number"), by.y=c("Fighter","Date", "Fight.Number"))


# Reorganizing the final data frame
final_file <- final_file %>%
  select("Promotion", "Event", "EventID", "Date", "Fight.Number", "Fighter", "FighterID", "FighterBirthdate", "FighterAge", "FighterWeight",
         "FighterPreFightScore", "FighterPostFightScore", "Opponent", "OpponentID", "OpponentBirthdate","OpponentAge", "OpponentWeight",
         "OpponentHeight","Result", "Method", "Round", "Time","OpponentPreFightScore", "OpponentPostFightScore") %>%
  mutate(FighterAge = str_sub(FighterAge, end = -2),
         OpponentAge = str_sub(OpponentAge, end = -2))

write.csv(final_file, paste0("./FinalFiles/MMAFightsScores", date, ".csv"), row.names=F)
