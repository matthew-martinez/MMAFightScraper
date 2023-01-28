library(rvest)
library(httr)
library(stringr)
library(tidyverse)

saveDir <- "~/Documents/Projects/Data/MMA/"

date <- Sys.Date()

# Bringing in old fighter stats
base <- read.csv(paste0(saveDir, "MMA_Fighter_StatsJan2023.csv"))[,2:5] # base fighter stats (last iteration)

new <- read.csv(paste0(saveDir, "Revent-records95784.csv")) # most recent event scrape

d <- new %>% filter(FighterID %in% base$FighterId |
                      OpponentID %in% base$FighterId)

# Only interested in UFC and Bellator fights
dSelect <- d %>%
  filter(str_detect(Promotion, "Ultimate Fighting Championship") | 
           str_detect(Promotion, "Bellator") |
           str_detect(Promotion, "Professional Fighters League") |
           str_detect(Promotion, "One Championship"))

# Making vectors of fighter and opponent IDs and bringing them together
fighterIDs <- unique(dSelect$FighterID)
opponentIDs <- unique(dSelect$OpponentID)

allIDs <- c(fighterIDs, opponentIDs)

allIDs <- unique(allIDs)

# Setting up the loop
i <- 0
fullRecords <- data.frame()[1:4,]

startTime <- Sys.time()

# loop through as many iterations as length of IDs, pulling fighter stats from each URL
# Looking at getting the birthdate, weight, and height
for (i in 1:length(allIDs)) {
  # loop number
  print(i)
  
  # creating the sherdog URL
  site <- NULL
  sitePaste <- paste("https://www.sherdog.com/fighter/", (allIDs[i]), sep="")
  print(sitePaste)

  urlCheck <- http_error(sitePaste)
  
  if (urlCheck == FALSE)
  {
    site <- html_session(sitePaste)
  
    birthDate <- site %>% html_nodes(".bio-holder") %>% html_nodes("span") %>% html_text(trim=TRUE)
    birthDate <- birthDate[1] # some <span>s come with multiple items beyond birth date
    height <- site %>% html_nodes(".bio-holder") %>% html_nodes("[itemprop='height']") %>% html_text(trim=TRUE)
    weight <- site %>% html_nodes(".bio-holder") %>% html_nodes("[itemprop='weight']") %>% html_text(trim=TRUE)
    
    if (identical(height, character(0))) { 
      height <- NA
    } 
    if (identical(weight, character(0))) { 
      weight <- NA
    } 
    
    fighterStats <- data.frame(FighterId = allIDs[i], Birthdate = birthDate, Height=height, Weight=weight)
    fullRecords <- rbind(fullRecords, fighterStats)
  }
}

endTime <- Sys.time()
endTime - startTime

final <- rbind(base, fullRecords) # binds together the base file and the new fighter stats

write.csv(final, paste0(saveDir, "MMA_Fighter_Stats", date, ".csv"))