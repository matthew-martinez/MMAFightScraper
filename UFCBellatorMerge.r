library(tidyverse)

# original sherdog scape
eventRecords <- read.csv("/home/m/Documents/R/MMA/data/eventRecords_12-20-draws-fixed.csv")[,2:14]
eventRecords$Date <- as.Date(eventRecords$Date, "%m/%d/%Y")
eventRecords <- eventRecords %>%
  filter(Promotion == "Ultimate Fighting Championship (UFC)" | Promotion == "Bellator MMA")

# second pull of Sherdog events (only new)
eventRecords2 <- read.csv("/home/m/Documents/R/MMA/data/2-15-scrape.csv")[,2:14]
eventRecords2 <- eventRecords2 %>% rename(Fight.Number = Match) %>% filter (EventID != 8788)
eventRecords2$Date <- as.Date(as.character(eventRecords2$Date), "%Y-%m-%d")

# keep adding new files as more events happen and are scraped

# binding event records together
eventRecords <- rbind(eventRecords, eventRecords2)

# similar to the above code - fighter stats for Bellator and UFC - keep adding new fighters as events/fights are added
dStats <- read.csv("/home/m/Documents/R/MMA/UFCBellatorFights.csv")[,2:5]
dStats2 <- read.csv("/home/m/Documents/R/MMA/UFCBellatorFights2.csv")[,2:5]

# bind them together
dStats <- rbind(dStats, dStats2)

dStats <- dStats %>% rename("FighterID" = "FighterId",
                            "FighterBirthdate" = "Birthdate") %>%
  select(FighterID, FighterBirthdate)
dStats$FighterBirthdate[dStats$FighterBirthdate == "N/AAGE: N/"] <- NA
dStats$FighterBirthdate <- as.Date(dStats$FighterBirthdate, "%Y-%m-%d")

# Recreating stats for each opponentID
dStatsOpponent<- dStats %>%
  select(FighterID, FighterBirthdate) %>%
  rename("OpponentID" = "FighterID",
         "OpponentBirthdate" = "FighterBirthdate")

# Merging stats for each fight - needs to be done twice, for the fighter and opponent
eventRecords <- merge(eventRecords, dStats, by="FighterID", all=FALSE)
eventRecords <- merge(eventRecords, dStatsOpponent, by="OpponentID", all=FALSE)

# Keeping on distinct rows
eventRecords <- eventRecords %>%
  distinct()

# Calculating age at time of fight - outputs in number of days, dividing and rounding to get years.
eventRecords$FighterAge <- round((eventRecords$Date - eventRecords$FighterBirthdate)/365,2)
eventRecords$OpponentAge <- round((eventRecords$Date - eventRecords$OpponentBirthdate)/365,2)

# Reorganizing the final data frame
eventRecords <- eventRecords %>%
  select("Promotion", "Event", "EventID", "Date", "Fight.Number", "Fighter", "FighterID", "FighterBirthdate", "FighterAge", "Opponent",
         "OpponentID", "OpponentBirthdate","OpponentAge", "Result", "Method", "Round", "Time")

# elo scores file
elo <- read.csv("/home/m/Documents/R/MMA/data/scoresfinalFullFebruary2021.csv")[,2:6]
elo$Date <- as.Date(elo$Date, "%Y-%m-%d")

fighterElo <- elo %>%
  filter(is.na(Date) == FALSE) %>%
  rename(FighterPreFightScore = PreFightScore,
         FighterPostFightScore = Score)

opponentElo <- elo %>%
  filter(is.na(Date) == FALSE) %>%
  rename(OpponentPreFightScore = PreFightScore,
         OpponentPostFightScore = Score)

eventRecordsElo <- merge(eventRecords, fighterElo, by.x=c("FighterID", "Date", "Fight.Number"), by.y=c("Fighter","Date", "Fight.Number"))

eventRecordsElo <- merge(eventRecordsElo, opponentElo, by.x=c("OpponentID", "Date", "Fight.Number"), by.y=c("Fighter","Date", "Fight.Number"))

eventRecordsElo <- eventRecordsElo %>%
  select("Promotion", "Event", "EventID", "Date", "Fight.Number", "Fighter", "FighterID", "FighterBirthdate", "FighterAge", "FighterPreFightScore", "Opponent",
         "OpponentID", "OpponentBirthdate","OpponentAge", "OpponentPreFightScore", "Result", "Method", "Round", "Time", "FighterPostFightScore", "OpponentPostFightScore") %>%
  arrange(Date, Fight.Number)

write.csv(eventRecordsElo, "/home/m/Documents/R/MMA/EventsRecordsElo_UFCBellatorFebruary2021.csv")
