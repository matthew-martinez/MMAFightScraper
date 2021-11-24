library(rvest)
library(httr)
library(stringr)
library(tidyverse)

######### FIGHTER RECORDS ###########

# initializing some empty objects for the loop
# counter can be set for where to start and where to end w/ sherdog URL
counter <- c(1:5000)
i <- 0
fullRecords <- data.frame()[1:8,]

startTime <- Sys.time()

# loop through as many interations as the above counter specifies
for (i in counter) {

  # creating the sherdog URL
  site <- NULL
  sitePaste <- paste("https://www.sherdog.com/fighter/x-x-", (counter[i]), sep="")
  print(sitePaste)
  
  site <- html_session(sitePaste)
  
  # grabbing all tables from the website
  tables <- tryCatch(html_nodes(site, "table"), error=function(e){NA})
  
  if (is.na(tables) == FALSE) {
    # reading table 1 (.[1]) and turning it into a list of data frames
    tablesParsed <- site %>%
      html_nodes("table") %>%
      .[1] %>%
      html_table(fill = TRUE)
    
    # need to fix it so that if there is an upcoming fight, it pulls from the 2nd table
    if (sum(grepl("Result", tablesParsed[[1]]$X1)) == 0){
      tablesParsed <- site %>%
        html_nodes("table") %>%
        .[2] %>%
        html_table(fill = TRUE)
    }
    
    # selecting only the first element of the list of data frames and creating an individual fight record
    fightRecord <- tablesParsed[[1]]
    
    if (is.na(fightRecord$X1) == FALSE){
      fighterName <- html_nodes(site, ".fn") %>% html_text
      fighterID <- counter[i]
      fightRecord <- cbind(fighterID, fighterName, fightRecord)
      
      colnames(fightRecord) <- c("FighterID", "Fighter", "Result", "Opponent", "Event", "Method", "Round", "Time")
      fightRecord<- fightRecord[2:nrow(fightRecord),]
      
      # binding the individual fight record to the fullRecord data frame
      # this will keep adding each new fight record for every iteration of the loop
      fullRecords <- rbind(fullRecords, fightRecord)
    }
  }
}

# creating column names and removing first row from scrape

endTime <- Sys.time()
endTime - startTime

# creating a copy of df for manipulating
fullRecords2 <- fullRecords

# creating a new date variable and formatting
fullRecords2$Date <- str_sub(fullRecords2$Event, start=-15)
fullRecords2$Date <- gsub(" ", "", fullRecords2$Date, fixed = TRUE)
fullRecords2$Date <- as.Date(fullRecords2$Date, "%b/%d/%Y")

fullRecords2$Event <- str_sub(fullRecords2$Event, end = nchar(fullRecords2$Event)-15)

# still working on this, need a few exceptions for Draw, Decision, and KO

FullRecords2$Method2 <- str_sub(fullRecords2$Method, end=str_locate(fullRecords2$Method, "\\)")[,1])

write.csv(fullRecords2, "/home/m/Documents/R/MMA/fullRecordsFirst500.csv")