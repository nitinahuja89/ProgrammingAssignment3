best <- function(state, outcome){
  #Domain data
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  mortalitycolumns <- c(11,17,23)
  hospitalnamecolumn <- 2;
  
  #Read outcome data
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Check that the state and outcome are valid
  states <- levels(as.factor(outcomedata$State))
  if(!is.element(state, states)){
    stop('invalid state')
  }
  if(!is.element(outcome, outcomes)){
    stop('invalid outcome')
  }
  
  #Return hospital name in that state with lowest 30-day death
  mortalitycolumn <- mortalitycolumns[match(outcome, outcomes)]
  outcomedata <- subset(outcomedata, outcomedata$State == state)
  outcomedata[,mortalitycolumn] <- as.numeric(outcomedata[,mortalitycolumn])
  completeVec <- complete.cases(outcomedata[, mortalitycolumn])
  outcomedata <- outcomedata[completeVec, ]
  sort(outcomedata[which(outcomedata[,mortalitycolumn] == min(outcomedata[,mortalitycolumn], na.rm = TRUE)), hospitalnamecolumn])[1]
}