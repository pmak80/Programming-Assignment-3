## Finding the Best Hospital in a state

best <- function(state, outcome){
  
  ## Read outcome data from csv file "outcome-of-care-measures.csv"
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  

  ##Check that state and outcome are valid
  if(state %in% state.abb){
    ##Extract the state subset
    ret_data <- subset(my_data, State == state)
  } else {
    stop('invalid state')
  }


  ##Return the column number from the outcome parameter passed-in and outcome are valid
  if(outcome == "heart attack"){
      col_num <- 13
      ret_data[ , 13] <- as.numeric(ret_data[ , 13]) ## Heart Attack lowest morality
  } else if(outcome == "heart failure"){
      col_num <- 19
      ret_data[ , 19] <- as.numeric(ret_data[ , 19]) ## Heart Failure lowest morality
  } else if(outcome == "pneumonia"){
     col_num <- 25  
     ret_data[ , 25] <- as.numeric(ret_data[ , 25]) ## Pneumonia lowest morality
     
  } else {
    stop('invalid outcome')
  }
  
  ret_data <- na.omit(ret_data)
  
  ## Sort my column (outcome) and hospital name and return the first row hospital name
  ret_data <- ret_data[order(ret_data[,col_num], ret_data[,2], na.last=TRUE),]
  
  ## return hospital name in that state with lowest 30-day death rate
  ret_data[1, 2]
  
 
}