##suppressWarnings(install.packages("stringr"))
##suppressWarnings(library(stringr))

rankhospital <- function(state, outcome, num) {
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
    mortidx <- 17
  } else {
    stop("invalid outcome")
  }
  suppressWarnings(ha <- data.frame(outcomedb[,7], as.numeric(outcomedb[,mortidx]), outcomedb[,2]))
  ha1 <- na.omit(ha)
  colnames(ha1) <- c("State", "Rate", "Hospital")
  ha1al <- ha1[ha1$State == stateu,]
  ha2al <- ha1al[order(ha1al$Rate, ha1al$Hospital),]
  n <- length(ha2al$Rate)   ## number of ratings
  
  ## need to search for ties, if so need to return all ties, sorted by hospital
  ## name
  
  if(is.numeric(num)) {
    if(num > n) {
      return(NA)   ## more hospitals with data
    }
  } else if ( num == "best") {
    num <- 1
  } else {
    num <- n   #assume worse
  }
  return(as.vector(as.character(ha2al[num,"Hospital"])))
}