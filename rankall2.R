rankall2 <- function(outcome, num="best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  diseases <- c("heart attack","heart failure", "pneumonia")
  states <- unique(data[, 7])
  
  ## Initialize Data Frame
  df <- data.frame()
  
  ## Check that outcome is valid
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
 
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  if(num=="best") num<-1
  
  for(state in states) {
      red1 <- reduced[reduced$State==state,]
      suppressWarnings(red1[,outcome] <- as.numeric(levels(red1[,outcome]))[red1[,outcome]])
      red1 <- red1[complete.cases(red1),]
      ordered <- red1[order(red1[outcome],red1["Name"]),]
      if(num=="worst") {
          num<-dim(ordered)[1]
          df <- rbind(df, ordered[num,1:2])
          num <- "worst"
      }
      else if(num>dim(ordered)[1]) {
          df <- rbind(df,c(NA,state))
      }
      
      else {
          df <- rbind(df, ordered[num,1:2])
      }
      
  }
  
  ## Return
  df <- df[order(df["State"]),]
  colnames(df) <- c("hospital","state")
  rownames(df) <- df[,"state"]
  df
}