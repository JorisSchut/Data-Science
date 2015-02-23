rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcomes <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings=c("Not Available"))
  outcomes <- outcomes[,c(2,7,11,17,23)]
  #col 1 = names, 2 = stat, 3= heart attack, 4 = haeart failure, 5 = pneumonia
  
  outcomes <- subset(outcomes, outcomes[2]==state)
  outcomes[, 3] <- as.numeric(outcomes[, 3])
  outcomes[, 4] <- as.numeric(outcomes[, 4])
  outcomes[, 5] <- as.numeric(outcomes[, 5])
  state_names <- unique(outcomes[ ,2])
  col_name<-numeric()
  
  if(state %in% state_names== FALSE) {
    stop("invalid state")
  }
  
  if (!(outcome %in% c("pneumonia","heart failure","heart attack"))) {
    stop("invalid outcome")
  }
  
  if(state %in% state_names== TRUE){
    
    if(num == "best"){
      num <- 1
    }
    
    if(num == "worst"){
      
      if(outcome== "heart attack"){
        col_name<-3
        num2<- sum(is.na(outcomes[col_name])==FALSE)
        num<-num2
      }
      if(outcome=="heart failure"){
        col_name<-4
        num2<- sum(is.na(outcomes[col_name])==FALSE)
        num<-num2
      }
      if(outcome=="pneumonia"){
        col_name<-5
        num2<- sum(is.na(outcomes[col_name])==FALSE)
        num<-num2
      }
      
    }
    
    if(num>nrow(outcomes)){
      return(NA)
    }
      
    if(outcome == "heart attack"){
      col_name<-3                
    }
    
    if(outcome == "heart failure"){
      col_name<-4
    }
    
    if(outcome == "pneumonia"){
      col_name<-5
    }
  
    outcomes2<-order(outcomes[,col_name], outcomes[,1])
    outcomes=outcomes[outcomes2,]
    return(outcomes[num,1])
  }
  
}