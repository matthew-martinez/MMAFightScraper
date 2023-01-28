###### ----------------------------------------------
# SETUP
#Elo script from https://raw.githubusercontent.com/infinitelytight/mma-elo/master/mma.r
###### ----------------------------------------------
library(tidyverse)
setwd("/home/m/Documents/Projects/Data") # change to directory you want to work/save files in
eventFile <- "/home/m/Documents/Projects/Data/MMA/eventRecords_2022_combined.csv" # change to path for full records .csv

###### ----------------------------------------------
# FUNCTIONS
###### ----------------------------------------------
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

###### ----------------------------------------------
# LOADING DATA
###### ----------------------------------------------
results <- read.csv(eventFile)
results$Date <- as.Date(results$Date, "%m/%d/%Y")

#extra results
# extra_results <- read.csv("results_extra.csv")

#bind multiple data sets
# results <- rbind(scraped_results, extra_results)

results <- results[order(results$Date, results$Fight.Number), ]

all_names <- c(as.character(results$FighterID), as.character(results$OpponentID))
fighters <- unique(all_names)

scores <- data.frame(fighters, 1000, 1000, NA, NA)
names(scores) <- c("Fighter", "PreFightScore", "Score", "Date", "Fight.Number")
rownames(scores) <- scores$Fighter

scoresFinal <- scores

###### ----------------------------------------------
# CALCULATING
###### ----------------------------------------------
startTime <- Sys.time()
for (i in 1:nrow(results)) {
  print(i)
  scores <- update_scores(results[i, ], scores)
  scoresFinal <- rbind(scoresFinal, scores)
  scoresFinal <- scoresFinal %>%
    filter(is.na(Score) == FALSE) %>%
  distinct()
}

scoresFinal$Date <- format(as.Date(scoresFinal$Date,origin="1970-01-01"))

endTime <- Sys.time()
endTime - startTime

ranked <- scores[order(-scores$Score), ]
ranked$Date <- format(as.Date(ranked$Date,origin="1970-01-01"))

top <- ranked[1:30, ]
print("Top 30:")
print(top, row.names=FALSE)

###### ----------------------------------------------
# SAVING - change file names if needed
###### ----------------------------------------------
save_rankings(scoresFinal, "scoresRanking_Jan072023.txt")
write.csv(scoresFinal, "scores_Jan072023.csv")
save_rankings(ranked, "rankings_Jan072023.txt")