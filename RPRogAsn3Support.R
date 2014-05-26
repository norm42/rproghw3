setwd("C:/Users/Norm/Desktop/MyFiles/Courses/RProgramming/RProgHw3")
outcome <- read.csv("outcome-of-care-measures.csv")
hospitals <- read.csv("hospital-data.csv")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
out30day <- as.numeric(outcome[,11])
"DC" %in% statevec
str_detect(diagname, "mortality..rates.from." )
str_locate(diagname[11], "mortality..rates.from.")
str_sub(diagname[11],45)

install.packages("stringr")
library(stringr)

suppressWarnings(ha <- data.frame(outcomedb[,7], as.numeric(outcomedb[,11]), outcomedb[,2]))
ha1 <- na.omit(ha)
colnames(ha1) <- c("State", "Rate", "Hospital")
ha1al <- ha1[ha1$State == "AL",]
ha2al <- ha1al[order(-ha1al$Rate),]

diagname <- tolower(names(outcomedb))   ## get the names in the list
diseasevec <- str_detect(diagname, "mortality..rates.from." )
n <- length(diseasevec)
diseasename <- vector(mode="character", length = n) 
j <- 1
for(i in 1:n) {
  if(diseasevec[i]) {
    idx <- str_locate(diagname[i], "mortality..rates.from.")
    diseasename[j] <- str_sub(diagname[i],idx[2]+1)
    j <- j+1
  }
}
dv <- unique(diseasename[1:(j-1)])
return(dv)
