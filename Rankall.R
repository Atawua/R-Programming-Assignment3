## rankall.R
rankall <- function(outcome, num = "best") {
     
     ## read my .csv data
     mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
     
     ## order my data first by State,secondly by the values of outcome and then by hospital names removing NA's
     ordered_data <- mydata[order(mydata[,7], as.numeric(mydata[,index]), mydata[,2], na.last = NA, decreasing = FALSE),]
     
     ## assign the unique values of all the States
     state <- unique(ordered_data[,7])
     
     ## create an empty character variable to use in for loop
     hospital <- character(0)
     
     ##  sort my data depending on the State column and assign the respective rankings to hospital character variable
     for(i in seq_along(state)) {
          sorted_data <- ordered_data[ordered_data[,7] == state[i],]
          translated_number <- num
          
          ## translate the num input to number if character
          if (translated_number == "best") {
               translated_number <- 1
          }
          if (translated_number == "worst") {
               
               translated_number <- nrow(sorted_data)
          }
          hospital[i] <- sorted_data[translated_number,"Hospital Name"]
     }
     ## print our data frame with the first column named "hospital" and second column as "state"
     data.frame(hospital = hospital, state = state)
}