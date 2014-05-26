##suppressWarnings(install.packages("stringr"))
##suppressWarnings(library(stringr))

best <- function(state, outcome) {
  ## read outcome data
  stateu <- toupper(state)
  outcomenb <- str_trim(outcome, side="both")
  outcomenb <- tolower(outcomenb)
  outcomedb <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  statevec <- unique(outcomedb[,7])  ## col 7 is the state
  if(!(stateu %in% statevec)){   ## is state valid
    stop("invalid state")
  }
  
  if(outcomenb == "heart attack") {
    mortidx <- 11
  } else if (outcomenb == "pneumonia") {
    mortidx <- 23
  } else if (outcomenb == "heart failure") {
    mortidx <- 18
  } else {
    stop("invalid outcome")
  }
  suppressWarnings(ha <- data.frame(outcomedb[,7], as.numeric(outcomedb[,mortidx]), outcomedb[,2]))
  ha1 <- na.omit(ha)
  colnames(ha1) <- c("State", "Rate", "Hospital")
  ha1al <- ha1[ha1$State == stateu,]
  ha2al <- ha1al[order(ha1al$Rate, ha1al$Hospital),]
  n <- length(ha2al$Rate) - 1  ## number of ratings
  j <- 1
  ## need to search for ties, if so need to return all ties, sorted by hospital
  ## name
  for(i in 1:n) {
    if(ha2al$Rate[1] != ha2al$Rate[i]) {
      break
    }
    j <- j+1   ## increment for ties
  }
  j <- j-1  ## adjust for loop increment
  return(as.vector(as.character(ha2al[1:j,"Hospital"])))
}