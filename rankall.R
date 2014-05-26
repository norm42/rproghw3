##suppressWarnings(install.packages("stringr"))
##suppressWarnings(library(stringr))

rankall <- function( outcome, num = "best") {
  ## read outcome data
  
  outcomenb <- str_trim(outcome, side="both")
  outcomenb <- tolower(outcomenb)
  outcomedb <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  statevec <- unique(outcomedb[,7])  ## col 7 is the state
  
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
  
  ## need to search for ties, if so need to return all ties, sorted by hospital
  ## name
  

  statevec <- unique(outcomedb[,7])  ## col 7 is the state
  ns <- length(statevec) 
  ## create vectors to hold results
  hospvec <- vector(mode="character", length = ns)
  for(i in 1:ns){
    ha1al <- ha1[ha1$State == statevec[i],]
    ha2al <- ha1al[order(ha1al$Rate, ha1al$Hospital),]
    n <- length(ha2al$Rate)   ## number of ratings
    if(is.numeric(num)) {
      if(num > n) {
        hospvec[i] <- NA   ## more than hospitals with data
      } else {
        hospvec[i] <- as.character(ha2al[num,"Hospital"])   #num ranking
      }
    } else if ( num == "best") {
      hospvec[i] <- as.character(ha2al[1,"Hospital"])
    } else if (num == "worst") {
      hospvec[i] <- as.character(ha2al[n,"Hospital"])   #assume worse
    } 
  }
  dfhosp <- data.frame( hospvec, statevec)
  colnames(dfhosp) <- c( "hospital", "state")
  dfret <- dfhosp[order(dfhosp$state, dfhosp$hospital),]
  return(dfret)
}