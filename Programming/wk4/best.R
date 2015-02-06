best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
 
  outcomes <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings=c("Not Available"))
  state_names <- unique(outcomes[ ,7])
  col_name<-numeric()
    
  if (!(outcome %in% c("pneumonia","heart failure","heart attack"))) {
    stop("invalid outcome")
  }
  
  if(state %in% state_names== FALSE) {
    stop("invalid state")
  }
  
  if(state %in% state_names== TRUE){ 
    
    outcomes <- subset(outcomes, outcomes[7]==state)
        
    if(outcome == "heart attack"){
      col_name<-11
    }
    
    if(outcome == "heart failure"){
     col_name<-17
    }
    
    if(outcome == "pneumonia"){
      col_name<-23     
    }

    outcomes[, col_name] <- as.numeric(outcomes[, col_name])
    minimum <- min(outcomes[col_name], na.rm=TRUE)
    outcomes <- subset(outcomes, outcomes[col_name]==minimum)
    return (as.character(outcomes[2]))
      
  }
  
}