## Randing hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best"){
  
  ##Validate State and outcome using the best function
  ##In the best function it will check the state and outcome are valid
  best(state, outcome)
  
  ## Read outcome data from csv file "outcome-of-care-measures.csv"
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Return the column number from the outcome parameter passed-in  
  if(outcome == "heart attack"){
    col_num <- 11
    my_data[ , 11] <- as.numeric(my_data[ , 11]) ## Heart Attack lowest morality
    if(num == "worst"){
      col_num <- 14  
      my_data[ , 14] <- as.numeric(my_data[ , 14]) ## Heart Attack highest morality
    }
  } else if(outcome == "heart failure"){
    col_num <- 17
    my_data[ , 17] <- as.numeric(my_data[ , 17]) ## Heart Failure lowest morality
    if(num == "worst"){
      col_num <- 20
      my_data[ , 20] <- as.numeric(my_data[ , 20]) ## Heart Failure highest morality
    }
  } else if(outcome == "pneumonia"){
    col_num <- 23
    my_data[ , 23] <- as.numeric(my_data[ , 23]) ## Pneumonia lowest morality
    if(num == "worst"){
      col_num <- 26
      my_data[ , 26] <- as.numeric(my_data[ , 26]) ## Pneumonia highest morality
    }
  } 
  my_data <- na.omit(my_data)

  ret_data <- subset(my_data, State == state)
  ret_data <- na.omit(ret_data)
  
  if(num == "best"){
    row_num <- 1
  } else if (num == "worst"){
    row_num <- nrow(ret_data)
  } else {
    row_num <- num
  }

  ## Return hospital name in that state withthe given rank (outcome)
  ## 30-day death rate
  if (row_num > nrow(ret_data)){
    result <- NA
  } else {
    ## Sort my column (outcome) and hospital name and return the first row hospital name
    ret_data <- ret_data[order(ret_data[,col_num], ret_data[,2], na.last=TRUE),]
    result <- ret_data[row_num, 2]
  }
  
  result
  
}