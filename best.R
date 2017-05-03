## Programs for week 4 R Programming Coursera Assignment

## best takes 2 arguments: the 2-char abbreviated name
## of a state and an outcome name.  THe function reads
## the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that
## has the best (lowest) 30-day mortality for specified
## outcome in the state
## 
## In the case of a tie, the hospital names should be
## sorted alphabetically, and the first hospital in that
## set should be chosen.

best <- function(state, outcome) {
    ## Read outcome data
    outcome_data  <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    state_list <- unique(outcome_data$State)
    if(!(state %in% state_list)) {
        stop("invalid state")
    }
    outcome_list <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% outcome_list)) {
        stop("invalid outcome")
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## Take subset of data where your values are in the state given to the function
    state_outcome <- outcome_data[outcome_data$State == state,][, c(2, 13, 19, 25)]
    
    #Assign empty vector
    hosp_name <- c()
    
    if(outcome == outcome_list[1]) {
        ## heart attack outcome
        
        state_outcome[, 2] <- as.numeric(state_outcome[, 2])
        good <- complete.cases(state_outcome)
        state_outcome <- state_outcome[good,]
        
        min_ha <- min(state_outcome[, 2], na.rm = TRUE)
        hosp_name <- state_outcome[state_outcome[, 2] == min_ha, 1][1]
    }
    
    if(outcome == outcome_list[2]) {
        ## heart failure outcome.
        
        state_outcome[, 3] <- as.numeric(state_outcome[, 3])
        good <- complete.cases(state_outcome)
        state_outcome <- state_outcome[good,]
        
        min_hf <- min(state_outcome[, 3], na.rm = TRUE)
        hosp_name <- state_outcome[state_outcome[, 3] == min_hf, 1][1]
    }
    
    if(outcome == outcome_list[3]) {
        ## pneumonia outcome.
        
        state_outcome[, 4] <- as.numeric(state_outcome[, 4])
        good <- complete.cases(state_outcome)
        state_outcome <- state_outcome[good,]
        
        min_p <- min(state_outcome[, 4], na.rm = TRUE)
        hosp_name <- state_outcome[state_outcome[, 4] == min_p, 1][1]
    }
    
    ## return rate
    hosp_name
}