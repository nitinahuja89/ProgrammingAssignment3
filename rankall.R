rankall <- function(outcome, num = "best"){
  result <- data.frame(hospital = character(), state = character())
  
  #Domain data
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  mortalitycolumns <- c(11,17,23)
  hospitalnamecolumn <- 2;
  
  #Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Check that the outcome is valid
  if(!is.element(outcome, outcomes)){
    stop('invalid outcome')
  }
  
  #Return data frame with hospital name and state name
  outcomedata$State <- as.factor(outcomedata$State)
  states = levels(outcomedata$State)
  mortalitycolumn <- mortalitycolumns[match(outcome, outcomes)]
  outcomedata[,mortalitycolumn] <- as.numeric(outcomedata[,mortalitycolumn])
  completeVec <- complete.cases(outcomedata[, mortalitycolumn])
  outcomedata <- outcomedata[completeVec, ]
  for(state in states){
    statedata <- subset(outcomedata, outcomedata$State == state)
    result <- rbind(result, rankallhelper(statedata, num, mortalitycolumn, hospitalnamecolumn, state))
  }
  result
}

rankallhelper <- function(statedata, num, mortalitycolumn, hospitalnamecolumn, statename){
  statedata <- statedata[order(statedata[,mortalitycolumn], statedata[,hospitalnamecolumn]), ]
  rows = nrow(statedata)
  if (num == "best"){
    data.frame(hospital = statedata[1,hospitalnamecolumn], state = statename)
  }
  else if (num == "worst"){
    data.frame(hospital = statedata[rows,hospitalnamecolumn], state = statename)
  }
  else if (num >=1 && num <= rows){
    data.frame(hospital = statedata[num,hospitalnamecolumn], state = statename)
  }
  else{
    data.frame(hospital = NA, state = statename)
  }
}