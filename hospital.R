head(outcome_of_care_measures)
str(outcome_of_care_measures)
hist(as.numeric(outcome_of_care_measures[, 11]))
attach(outcome_of_care_measures)
names(outcome_of_care_measures)

`Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack`
outcome_of_care_measures[, 11] <- as.numeric(outcome_of_care_measures[, 11])

rates <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
rm(outcome_of_care_measures)

hist(as.numeric(rates[, 11]))
url = "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
download.file(url, destfile = "temp")
unzip("temp")
unlink("temp")

require(plyr)
## Loading required package: plyr
hosp_sort <- function(state,outcome){
  #setting NA to be 0
  rates[rates == "Not Available"] <- 0
  index <- c(grep("^Hospital.*Death*", names(rates)))
  mortality_rates <- rates[,c(2,7,index)]
  names(mortality_rates)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  
  #Making rates numeric
  mortality_rates[,3:5] <- apply(mortality_rates[,3:5],2,as.numeric)
  
  mortality_rates[mortality_rates == 0] <- NA
  selected_state <- mortality_rates[mortality_rates$State == state,]
  
  order_selected <- arrange(selected_state, selected_state[,outcome], Hospital.Name, na.last=TRUE)
  order_selected <- order_selected[complete.cases(order_selected[,outcome]),]
  return(order_selected)
  #na.omit(selected_state[order(c(selected_state[,outcome]), na.last = TRUE),])
}

best <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  return(order_selected[1,1])
  
}

library(dplyr)

best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
## [1] "Invalid State"
best("NY", "hert attack")
## [1] "Invalid Outcome"

worst <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  
  return(order_selected[nrow(order_selected),1])
}

rankhospital <- function(state, outcome, num = "best", st = "a"){
  if (num == "best")
    return(best(state,outcome))
  if (num == "worst")
    return(worst(state,outcome))       
  else {order_selected <- hosp_sort(state,outcome) 
  return(order_selected[num,1])}
}

rankhospital("TX", "heart failure", 4)

hosp_sort <- function(state,outcome){
  #setting NA to be 0
  rates[rates == "Not Available"] <- 0
  index <- c(grep("^Hospital.*Death*", names(rates)))
  mortality_rates <- rates[,c(2,7,index)]
  names(mortality_rates)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  
  #Making rates numeric
  mortality_rates[,3:5] <- apply(mortality_rates[,3:5],2,as.numeric)
  
  mortality_rates[mortality_rates == 0] <- NA
  selected_state <- mortality_rates[mortality_rates$State == state,]
  
  order_selected <- arrange(selected_state, selected_state[,outcome], Hospital.Name, na.last=TRUE)
  order_selected <- order_selected[complete.cases(order_selected[,outcome]),]
  return(order_selected)
  #na.omit(selected_state[order(c(selected_state[,outcome]), na.last = TRUE),])
}

best <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  return(order_selected[1,c(1,2)])
  
}

worst <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  
  return(order_selected[nrow(order_selected),c(1,2)])
}

rankhospital <- function(state, outcome, num = "best", st = "a"){
  if (num == "best")
    return(best(state,outcome))
  if (num == "worst")
    return(worst(state,outcome))       
  else {order_selected <- hosp_sort(state,outcome) 
  return(order_selected[num,c(1,2)])}
}

rankall <- function(outcome, num = "best") {
  #print(lapply(unique(rates$State),hosp_sort, outcome))
  results <- unlist(lapply(sort(unique(rates$State)), rankhospital, outcome, num),use.names=FALSE)
  hosp <- results[c(TRUE,FALSE)]
  state <- results[c(FALSE,TRUE)]
  all <- data.frame(hosp,state)
  names(all) <- c("Hospital.Name", "state")
  all
  
}

head(rankall("heart attack", 20), 10)
head(rankall("heart attack", 20), 10)
