rankhospital2 <- function(state,outcome,num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    states <- unique(data[, 7])
    diseases <- c("heart attack","heart failure", "pneumonia")

    ## Check that state and outcome are valid
    if(!(state %in% states)) {
        stop("invalid state")
    }
    if(!(outcome %in% diseases)) {
        stop("invalid outcome")
    }
    
    ## Get Data Nice
    ## col 7 is state
    ## col 2 is hospital name
    ## col 11 is heart attack
    ## col 17 is heart failure
    ## col 23 is pneumonia
    cleaned_data <- data[,c(2,7,11,17,23)]
    colnames(cleaned_data) <- c("Name","State","heart attack",
                                "heart failure", "pneumonia")
    reduced <- cleaned_data[,c("Name","State",outcome)]
    red1 <- reduced[reduced$State==state,]
    suppressWarnings(red1[,outcome] <- as.numeric(levels(red1[,outcome]))[red1[,outcome]])
    red1 <- red1[complete.cases(red1),]
    
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ordered <- red1[order(red1[outcome],red1["Name"]),]
    
    if(num=="worst") {
        return(as.character(ordered[dim(ordered)[1],1]))
    }
    if(num=="best") {
        return(as.character(ordered[1,1]))
    }
    ## See if num is too long
    if(num>length(red1[,"State"])) {
        return(NA)
    }
    as.character(ordered[num,1])
}