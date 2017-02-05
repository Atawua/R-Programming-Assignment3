## rankhospital.R
rankhospital <- function(state, outcome, num = "best") {
     
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
     
     ## order the subset first by outcome values and then by hospitals name without NA's
     ordered_data <- subdata[order(as.numeric(subdata[,index]), subdata[,2], na.last = NA, decreasing = FALSE),]
     
     ## translate the num input to numeric if it is put as character
     if(num == "best") {
          num <- 1
     }
     if(num == "worst") {
          num <- length(ordered_data[,index])
     }
     
     ## if num value is bigger that our observations assign NA
     if(num > length(ordered_data[,index])) {
          ordered_data[num,2] <- NA
     }
     
     ## print the desired hospital name according to the desired rank
     ordered_data[num,2]
}