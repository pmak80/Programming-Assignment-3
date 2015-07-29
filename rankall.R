##Ranking hospitals in all States
## For each state, find the hospital of the given rank
rankall <- function(outcome, num = "best"){
  
  ## Read outcome data from csv file "outcome-of-care-measures.csv"
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##Return the column number from the outcome parameter passed-in and outcome are valid
  if(outcome == "heart attack"){
    col_num <- 11 
    my_data[ , 11] <- as.numeric(my_data[ , 11]) ## Heart Attack lowest morality
    
  } else if(outcome == "heart failure"){
    col_num <- 17
    my_data[ , 17] <- as.numeric(my_data[ , 17]) ## Heart Failure lowest morality
  } else if(outcome == "pneumonia"){
    col_num <- 23 
    my_data[ , 23] <- as.numeric(my_data[ , 23]) ## Pneumonia lowest morality
    
  } else {
    stop('invalid outcome')
  } 
  
  my_data <- na.omit(my_data)
  
  ##Column 2 - Name   Column 7 - State   Col_num - Outcome column    Split on State
  split_state<- split(my_data[ , c(2, 7, col_num)], my_data[ , 7]) 
  
  ##RANK FUNCITON TO CALL INTO *APPLY  ##1 column is Name, 2 is State and 3 is the outcome
  rank <- function(state){
    
    ret_data <- state[order(state[,3], state[,1], na.last=TRUE),]
    
    st <- state[1,2]
    
    if(num == "best"){
      row_num <- 1
    } else if (num == "worst"){
      row_num <- nrow(ret_data)
    } else {
      row_num <- num
    }
    
    
    if(row_num > nrow(ret_data)){
        result <- c(NA, st)
    } else {
      result <- ret_data[row_num, 1:2]
    }
      
    
    result
    
  }
  ## end of Rank Function
  
  ## Return a data frame with the hospital names and the (abbreviated) 
  ##  state name
  ret <- data.frame(t(sapply(split_state, rank)))
  ## name the columns in the data frame
  colnames(ret) <- c("hospital", "state")
  ret
}
