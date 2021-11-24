library(rvest)
library(httr)
library(stringr)
library(tidyverse)

# Bringing in event records
d <- read.csv("/home/m/Documents/R/MMA/data/eventRecordsFullClean.csv")[,2:14]

# Only interested in UFC and Bellator fights
dSelect <- d %>%
  filter(Promotion == "Ultimate Fighting Championship (UFC)" | Promotion == "Bellator MMA")

# Making vectors of fighter and opponent IDs and bringing them together
#fighterIDs <- unique(dSelect$FighterID)
#opponentIDs <- unique(dSelect$OpponentID)

# allIDs <- c(fighterIDs, opponentIDs)
# 
# allIDs <- unique(allIDs)

allIDs <- c("Tafon-Nchukwi-282049",
            "Jamie-Pickett-72595",
            "Sam-Hughes-231521",
            "Tecia-Torres-85096")

# Setting up the loop
i <- 0
fullRecords <- data.frame()[1:4,]

startTime <- Sys.time()

# loop through as many iterations as length of IDs, pulling fighter stats from each URL
# Looking at getting the birthdate, weight, and height (even though general weight and height may not be useful)
for (i in 1:length(allIDs)) {

  # creating the sherdog URL
  site <- NULL
  sitePaste <- paste("https://www.sherdog.com/fighter/", (allIDs[i]), sep="")
  print(sitePaste)

  site <- html_session(sitePaste)

  birthDate <- site %>% html_nodes(".birth_info") %>% html_text(trim=TRUE)
  birthDate <- str_sub(birthDate, start=7, end=16)
  
  size <- site %>% html_nodes(".size_info") %>% html_text(trim=TRUE)
  size <- strsplit(size, "\\\n")
  height <- as.numeric(str_sub(str_trim(size[[1]][2]), end=-4))
  weight <- as.numeric(str_sub(str_trim(size[[1]][5]), end=-4))
  
  fighterStats <- data.frame(FighterId = allIDs[i], Birthdate = birthDate, Height=height, Weight=weight)
  fullRecords <- rbind(fullRecords, fighterStats)
}

endTime <- Sys.time()
endTime - startTime

write.csv(fullRecords, "/home/m/Documents/R/MMA/UFCBellatorFights2.csv")
