## best.R
best <- function(state, outcome) {
     
     ## read my .csv data
     mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## if the inputed state is different from all states stop the process
     if(all(state != mydata[,7])) {
          stop(print("invalid state"))
     }
     stop <- 0
     
     ## find the correct outcome and index its column
     if(outcome == "heart attack") {
          index <- 11
          stop <- 1
     }
     if(outcome == "heart failure") {
          index <- 17
          stop <- 1
     }
     if(outcome == "pneumonia") {
          index <- 23
          stop <- 1
     }
     
     ## if counter is 0 that means that no outcome was found with such name so stop the process
     if(stop == 0) {
          stop(print("invalid outcome"))
     }
     ## subset the required data according to the desired state
     subdata <- subset(mydata, State == state)
     
     ## change the class of the outcome column from characters to numeric
     numericdata <- sapply(subdata[,index],as.numeric)
     
     ## find the rows where the min value was spotted(without NA's)
     rows <- which(numericdata == min(numericdata, na.rm = TRUE))
     
     ## assing the name of the hospitals with the minimum value to besthospitals
     besthospitals <- subdata[rows,2]
     
     ## if besthospitals are more than 1,print the hospital with the minimum value in terms of name
     if(length(besthospitals) > 1) {
          besthospitals <- min(besthospitals)
     }
     else {
          besthospitals
     }
}