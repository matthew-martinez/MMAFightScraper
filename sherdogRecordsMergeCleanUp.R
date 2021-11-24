library(tidyverse)

# This is for cleaning up the scraped event records file
# Making the Method variable readable and adding in fighter age at time of fight

# Reading in fully compiled event records from prior scrape
# Only interested in UFC and Bellator fights
d <- read.csv("/home/m/Documents/R/MMA/data/scrape-12-19-scrape-combined-NAremoved.csv")[,2:14]
d$Date <- as.Date(d$Date, "%m/%d/%y")

# d2 <- read.csv("/home/m/Documents/R/MMA/data/eventRecordsFullClean.csv")[,2:14]
# d2 <- d2 %>%
#  rename(Match = Fight.Number)
# d2$Date <- as.Date(d2$Date, "%m/%d/%y")
# 
# d <- rbind (d, d2)

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
dStats <- read.csv("/home/m/Documents/R/MMA/UFCBellatorFights.csv")[,2:5]
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
d <- merge(d, dStats, by="FighterID", all=FALSE)
d <- merge(d, dStatsOpponent, by="OpponentID", all=FALSE)

# Keeping on distinct rows
d <- d %>%
  distinct()

# Calculating age at time of fight - outputs in number of days, dividing and rounding to get years.
d$FighterAge <- round((d$Date - d$FighterBirthdate)/365,2)
d$OpponentAge <- round((d$Date - d$OpponentBirthdate)/365,2)

# Reorganizing the final data frame
d <- d %>%
  select("Promotion", "Event", "EventID", "Date", "Fight.Number", "Fighter", "FighterID", "FighterBirthdate", "FighterAge", "Opponent",
         "OpponentID", "OpponentBirthdate","OpponentAge", "Result", "Method", "Round", "Time")

write.csv(d, "/home/m/Documents/R/MMA/eventRecords_12292020.csv")
