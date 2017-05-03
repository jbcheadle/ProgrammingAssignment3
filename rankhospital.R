##Rank Hospital

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
  state_list <- unique(outcome_data$State)
  if(!(state %in% state_list)) {
      stop("invalid state")
  }
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% outcome_list)) {
      stop("invalid outcome")
  }
  
  ## Take subset of data where your values are in the state given to the function
  state_outcome <- outcome_data[outcome_data$State == state,][, c(2, 11, 17, 23)]
  ## Check first that num is a number, otherwise "best" = 1
  if(num == "best") num <- 1
  if(num != "worst" && num > dim(state_outcome)[1]) return(NA)
  
  ## and "worst" = tail(vector, n=1) or df[length(df)]
  ## heart attack = Col 11
  ## heart failure = Col 17
  ## pnemonia = Col 23
  
 
  
  #Assign empty vector
  hosp_name <- c()
  
  if(outcome == outcome_list[1]) {
      ## heart attack outcome
      
      state_outcome[, 2] <- as.numeric(state_outcome[, 2])
      good <- complete.cases(state_outcome)
      state_outcome <- state_outcome[good,]
      
      state_order <- state_outcome[order(state_outcome[,2], state_outcome[,1]), ]
      if(num == "worst") {
          hosp_name <- state_order[dim(state_order)[1],1]
      }
      else {
          hosp_name <- state_order[num,1]
      }
      
  }
  
  if(outcome == outcome_list[2]) {
      ## heart failure outcome.
      
      state_outcome[, 3] <- as.numeric(state_outcome[, 3])
      good <- complete.cases(state_outcome)
      state_outcome <- state_outcome[good,]
      
      state_order <- state_outcome[order(state_outcome[,3], state_outcome[,1]), ]
      if(num == "worst") {
          hosp_name <- state_order[dim(state_order)[1], 1]
      }
      else {
          hosp_name <- state_order[num,1]
      }
  }
  
  if(outcome == outcome_list[3]) {
      ## pneumonia outcome.
      
      state_outcome[, 4] <- as.numeric(state_outcome[, 4])
      good <- complete.cases(state_outcome)
      state_outcome <- state_outcome[good,]
      
      state_order <- state_outcome[order(state_outcome[,4], state_outcome[,1]), ]
      if(num == "worst") {
          hosp_name <- state_order[dim(state_order)[1],1]
          print(state)
      }
      else {
          hosp_name <- state_order[num,1]
      }
      
  }
  
  hosp_name
}