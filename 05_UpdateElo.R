# Elo code adapted from: https://raw.githubusercontent.com/infinitelytight/mma-elo/master/mma.r

#### ------------------------------------------------------
library(tidyverse)
library(openxlsx)
setwd("/home/m/Documents/Projects/Data") # change to directory you want to work/save files in
eventFile <- "/home/m/Documents/Projects/Data/MMA/eventRecords_2022_combined.csv" # change to path for full records .csv
eventFileUpdate <-"/home/m/Documents/Projects/Data/MMA/Revent-records95784.csv"

baseElo <- "/home/m/Documents/Projects/Data/MMA/scores_Jan072023.csv"

input <- ""
output <- ""

date <- Sys.Date()

#### ------------------------------------------------------
options(error=traceback)
options(max.print=999999)

get_score <- function(fighter, scores) {
  scores[fighter, ]$Score
}

update_scores <- function(fight, scores, k=100) {
  fighter <- as.character(fight$FighterID)
  opponent <- as.character(fight$OpponentID)
  result <- as.character(fight$Result)
  match <- fight$Fight.Number
  date <- fight$Date
  
  f_score <- get_score(fighter, scores)
  o_score <- get_score(opponent, scores)
  
  # Transformed rating for Elo algorithm
  f_rating <- 10 ^ (f_score / 400)
  o_rating <- 10 ^ (o_score / 400)
  
  # Expected score
  f_exp = f_rating / (f_rating + o_rating)
  o_exp = o_rating / (f_rating + o_rating)
  
  if (result == "win" || result == "Win") {
    f_result = 1
    o_result = 0
  }
  # "Fighter" never loses to "Opponent"
  # else if (result == "loss") {
  #  f_result = 0
  #  o_result = 1
  #}
  else if (result == "vs." || result == "Draw") {
    if (result == "No Contest" || result == "No contest") {
      return(scores)
    } else {
      f_result = 0.5
      o_result = 0.5
    }
  }
  else {
    return(scores)
  }
  
  f_new_score = round(f_score + k * (f_result - f_exp))
  o_new_score = round(o_score + k * (o_result - o_exp))
  
  scores[scores[, "Fighter"] == fighter, ]$PreFightScore <- f_score
  scores[scores[, "Fighter"] == opponent, ]$PreFightScore <- o_score
  scores[scores[, "Fighter"] == fighter, ]$Score <- f_new_score
  scores[scores[, "Fighter"] == fighter, ]$Date <- date
  scores[scores[, "Fighter"] == fighter, ]$Fight.Number <- match
  scores[scores[, "Fighter"] == opponent, ]$Score <- o_new_score
  scores[scores[, "Fighter"] == opponent, ]$Date <- date
  scores[scores[, "Fighter"] == opponent, ]$Fight.Number <- match
  
  print(fighter)
  print(date)
  print(match)
  
  return(scores)
}

save_rankings <- function(rankings, filename) {
  sink(filename)
  print(data.frame(Rank=1:nrow(rankings), rankings), row.names=FALSE)
  closeAllConnections()
  print(paste(c("Full list saved in", filename), collapse=" "))
}

# original event record file from Sherdog
results <- read.csv(eventFile)[,2:15]
results <- results %>% select(-New.ID)
results$Date <- as.Date(results$Date, "%m/%d/%Y")

# new scrape of events
results2 <- read.csv(eventFileUpdate)[,2:14]
results2 <- results2 %>% rename(Fight.Number = Match)
results2$Date <- as.Date(results2$Date, "%Y-%m-%d")

# binding original and the newer file - keep doing this for each additional file 
# (e.g., repeat lines 94-96, then bind all results# dfs together)
results <- rbind(results, results2)
results <- results[order(results$Date, results$Fight.Number), ]

# previously calculated scores file - we will search/append new fighters/scores/update scores to the original file
scores<- read.csv(baseElo)[,2:6]
scores$Date <- as.Date(as.character(scores$Date), "%Y-%m-%d")
scoresFinal <- scores

scores <- scores[order(scores$Date, scores$Fight.Number, decreasing=TRUE), ]
scores <- scores[!duplicated(scores$Fighter),]

# change results2 to the latest event file - need to make a list of new fighters to calc scores for
allNames <- c(as.character(results2$FighterID), as.character(results2$OpponentID))
namesNew <- allNames[allNames %in% scores$Fighter == FALSE]
namesNew <- unique(namesNew)
scoresNew <- data.frame(namesNew, 1000, 1000, NA, NA)
names(scoresNew) <- c("Fighter", "PreFightScore", "Score", "Date", "Fight.Number")
scores <- rbind(scores,scoresNew)
rownames(scores) <- scores$Fighter

startTime <- Sys.time()
for (i in 1:nrow(results2)) { # modifying nrow(results#) to reflect only the new fights to calc elos for
  print(i)
  scores <- update_scores(results2[i, ], scores)
  scoresFinal <- rbind(scoresFinal, scores)
  scoresFinal <- scoresFinal %>%
    filter(is.na(Score) == FALSE) %>%
    distinct()
}

endTime <- Sys.time()
endTime - startTime

ranked <- scores[order(-scores$Score), ]

top <- ranked[1:30, ]
print("Top 30:")
print(top, row.names=FALSE)

# Uncomment to save full list to file
save_rankings(scoresFinal, paste0("scoresRanking_", date, ".txt"))
write.csv(scoresFinal, paste0("scores_", date, ".csv"))
save_rankings(ranked, paste0("rankings_", date, ".txt"))
