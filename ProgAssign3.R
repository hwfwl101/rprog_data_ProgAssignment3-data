#BEST HOSPITAL

best<- function(state, outcome)
{
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  min_row <- which(as.numeric(outcome2[ ,colnum]) == 
                     min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
  hospitals <- outcome2[min_row,2]
  hospitals <- sort(hospitals)
  return(hospitals[1])
}

# example output:
best("SC", "heart attack")


#RANK HOSPITAL

rankhospital<- function(state, outcome, num = "best")
{
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  outcome3 <- outcome2[order(outcome2[ ,colnum],outcome2[,2]), ]
  outcome3 <- outcome3[(!is.na(outcome3[ ,colnum])),]
  if(num == "best"){
    num <- 1
  }            
  else if (num == "worst"){
    num <- nrow(outcome3)
  }      
  return(outcome3[num,2])
}

# example output:
rankhospital("NC", "heart attack", "worst")



#RANKALL
rankall<- function(outcome, num = "best")
{
  library(dplyr)
  library(magrittr)
  outcome2 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if((outcome %in% c("heart attack", "heart failure",
                     "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  
  outcome2 = outcome2[!is.na(outcome2[,colnum]),]
  
  splited = split(outcome2, outcome2$State)
  ans = lapply(splited, function(x, num) {
    x = x[order(x[,colnum], x$Hospital.Name),]
    
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}


# example output:
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
