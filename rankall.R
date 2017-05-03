## Rankall
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Subset only columns of interest
    outcome_data <- outcome_data[c(2,7,11,17,23)]
    
    
    ## Check that state and outcome are valid
    state_list <- unique(outcome_data$State)
    state_list <- state_list[order(state_list)]

    outcome_list <- c("heart attack", "heart failure", "pneumonia")
    if(!(outcome %in% outcome_list)) {
        stop("invalid outcome")
    }
    
    ## Check first that num is a number, otherwise "best" = 1
    if(num == "best") num <- 1
    
    ## and "worst" = tail(vector, n=1) or df[length(df)]
    ## heart attack = Col 11
    ## heart failure = Col 17
    ## pnemonia = Col 23
    
    #Assign empty data frame
    output_frame <- data.frame()
    
    if(outcome == outcome_list[1]) {
        ## heart attack outcome (Now col. 3)
        outcome_data[, 3] <- as.numeric(outcome_data[, 3])
        good <- complete.cases(outcome_data)
        outcome_data <- outcome_data[good,]
        
        hosp_names <- c()
        ## Loop through State
        for(state in state_list) {
            
            ## get outcome for state, and order list
            state_outcome <- outcome_data[outcome_data$State == state,][, c(1, 3)]
            state_order <- state_outcome[order(state_outcome[,2], state_outcome[,1]), ]
            
            
            if(num == "worst") {
            
                ##Find hosp Name
                hosp_names <- append(hosp_names,state_order[dim(state_order)[1],1])
            }
            else {
                if(num != "worst" && num > dim(state_outcome)[1]) hosp_names <- append(hosp_names,"<NA>")
                else {
                    
                    ##Find Hosp Name
                    hosp_names <- append(hosp_names,state_order[num,1])
                }
            }
            
            
        }
        print(hosp_names)
        output_frame <- cbind(hosp_names,state_list)
        
    }
    
    if(outcome == outcome_list[2]) {
        ## heart failure outcome (Now column 4)
        outcome_data[, 4] <- as.numeric(outcome_data[, 4])
        good <- complete.cases(outcome_data)
        outcome_data <- outcome_data[good,]
        
        hosp_names <- c()
        ## Loop through State
        for(state in state_list) {
            
            ## get outcome for state, and order list
            state_outcome <- outcome_data[outcome_data$State == state,][, c(1, 4)]
            state_order <- state_outcome[order(state_outcome[,2], state_outcome[,1]), ]
            
            
            if(num == "worst") {
                
                ##Find hosp Name
                hosp_names <- append(hosp_names,state_order[dim(state_order)[1],1])
            }
            else {
                if(num != "worst" && num > dim(state_outcome)[1]) hosp_names <- append(hosp_names,"<NA>")
                else {
                    
                    ##Find Hosp Name
                    hosp_names <- append(hosp_names,state_order[num,1])
                }
            }
            
            
        }
        output_frame <- cbind(hosp_names,state_list)
    }
    
    if(outcome == outcome_list[3]) {
        ## pneumonia outcome (now column 5).
        outcome_data[, 5] <- as.numeric(outcome_data[, 5])
        good <- complete.cases(outcome_data)
        outcome_data <- outcome_data[good,]
        
        hosp_names <- c()
        ## Loop through State
        for(state in state_list) {
            
            ## get outcome for state, and order list
            state_outcome <- outcome_data[outcome_data$State == state,][, c(1, 5)]
            state_order <- state_outcome[order(state_outcome[,2], state_outcome[,1]), ]
            
            
            if(num == "worst") {
                
                ##Find hosp Name
                hosp_names <- append(hosp_names,state_order[dim(state_order)[1],1])
            }
            else {
                if(num != "worst" && num > dim(state_outcome)[1]) hosp_names <- append(hosp_names,"<NA>")
                else {
                    
                    ##Find Hosp Name
                    hosp_names <- append(hosp_names,state_order[num,1])
                }
            }
            
            
            
        }
        output_frame <- cbind(hosp_names,state_list)
        
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    row.names(output_frame) <- state_list
    colnames(output_frame) <- c("hospital", "state")
    output_frame
}
