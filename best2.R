best2 <- function(state, outcome) {
    ##Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    states <- unique(data[, 7])
    diseases <- c("heart attack","heart failure", "pneumonia")
    
    ##Check that state & outcome are valid
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
    ##Return hospital name in that state with lowest
    ##30-day death rate
    suppressWarnings(red1[,outcome] <- as.numeric(levels(red1[,outcome]))[red1[,outcome]])
    red1 <- red1[complete.cases(red1),]
    ret <- red1[red1[,3]==min(red1[,3]),]
    as.character(ret$Name[1])

}