
checkResult <- function(r, name = c("best", "rankhospital", "rankall")) {
  name <- match.arg(name)
  if(name == "best" || name == "rankhospital") {
    if(length(r) == 1L && is.na(r))
      return(r)
    if(!is.character(r))
      stop(sprintf("'%s' did not return a character vector",
                   name))
    if(!length(r))
      stop(sprintf("'%s' returned character vector of length 0", name))
    if(length(r) > 1)
      stop(sprintf("'%s' returned a character vector of length > 1", name))
  }
  else if(name == "rankall") {
    if(!is.data.frame(r))
      stop(sprintf("'%s' did not return a data frame", name))
    if(ncol(r) != 2L)
      stop(sprintf("'%s' should return data frame with exactly 2 columns", name))
    if(!all(names(r) %in% c("hospital", "state")))
      stop("column names of data frame should be 'hospital' and 'state'")
  }
  r
}  

r1 <- rankhospital("NC", "heart attack", "worst")
checkResult(r1, "rankhospital")
 
  r2 <- rankhospital("WA", "heart attack", 7)
checkResult(r2, "rankhospital")

  r3 <-rankhospital("WA", "pneumonia", 1000)
checkResult(r3, "rankhospital")

  r4 <- rankhospital("NY", "heart attak", 7)
checkResult(r4, "rankhospital")
 
r5 <- rankhospital("TX", "heart failure", 4)
r6 <- rankhospital("NC", "heart attack", "worst")
r7 <- rankhospital("MN", "heart attack", 5000)