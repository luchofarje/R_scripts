##by Lucho Farje
##2014-05-29

best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",sep=",",colClasses="character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome_data[,7]))
    stop("invalid state")
  if(!(outcome %in% c('heart attack','heart failure','pneumonia')))
    stop("invalid outcome")
  outcome_data<-outcome_data[outcome_data$State==state,]
  suppressWarnings(outcome_data[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]<-sapply(outcome_data[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')],as.numeric))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if(outcome=='heart attack')
    {
      as.character(head(outcome_data[order(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),"Hospital.Name"],n=1))
    }
  else if(outcome=='heart failure')
    as.character(head(outcome_data[order(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),"Hospital.Name"],n=1))
  else if(outcome=='pneumonia')
    as.character(head(outcome_data[order(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),"Hospital.Name"],n=1))
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",sep=",",colClasses="character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome_data[,7]))
    stop("invalid state")
  if(!(outcome %in% c('heart attack','heart failure','pneumonia')))
    stop("invalid outcome")
  if(num=="best"){
    num=1
    desc=FALSE    
  }
  else if(num=="worst"){
    num=1
    desc=TRUE
  }
  else if(is.numeric(num))
    desc=FALSE
  else
    stop("invalid num")
  outcome_data<-outcome_data[outcome_data$State==state,]
  if(num>nrow(outcome_data))
    return(NA)
  suppressWarnings(outcome_data[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]<-sapply(outcome_data[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')],as.numeric))
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(outcome=='heart attack'){
    as.character(tail(head(outcome_data[order(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcome_data$Hospital.Name,decreasing=desc),"Hospital.Name"],n=num),n=1))
  }
  else if(outcome=='heart failure')
    as.character(tail(head(outcome_data[order(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcome_data$Hospital.Name,decreasing=desc),"Hospital.Name"],n=num),n=1))
  else if(outcome=='pneumonia')
    as.character(tail(head(outcome_data[order(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome_data$Hospital.Name,decreasing=desc),"Hospital.Name"],n=num),n=1))
}

source("rankhospital.R")
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv",sep=",",colClasses="character")
  
  ## Check that state and outcome are valid
  ##if(!(state %in% outcome_data[,7]))
  ##  stop("invalid state")
  ##if(!(outcome %in% c('heart attack','heart failure','pneumonia')))
  ##  stop("invalid outcome")
  
  hospitalranks<-data.frame()
  ## For each state, find the hospital of the given rank
  for(state in unique(outcome_data$State)){
    #hospitalranks<-rbind(hospitalranks,data.frame(hosrank(outcome,state,num),state))
    hospitalranks<-rbind(hospitalranks,data.frame(hospital=rankhospital(state, outcome, num),state=state))
  } 
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  hospitalranks[order(as.character(hospitalranks$state)),]
}