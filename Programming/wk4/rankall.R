rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
 
  outcomes <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings=c("Not Available"))
  outcomes <- outcomes[,c(2,7,11,17,23)]
  #col 1 = names, 2 = stat, 3= heart attack, 4 = haeart failure, 5 = pneumonia
  
  state_names <- unique(outcomes[ ,2])
  state_names2<-order(state_names)
  state_names=state_names[state_names2]
  
  data_frame<-data.frame()
  col_number<-numeric()
    
  outcomes[, 3] <- as.numeric(outcomes[, 3])
  outcomes[, 4] <- as.numeric(outcomes[, 4])
  outcomes[, 5] <- as.numeric(outcomes[, 5])
    
    if(num == "best"){
      num <- 1
    }
  
  if (!(outcome %in% c("pneumonia","heart failure","heart attack"))) {
    stop("invalid outcome")
  }
  
  if(outcome == "heart attack"){
    col_number<-3
  }
  
  if(outcome == "heart failure"){
    col_number<-4
  }
  
  if(outcome == "pneumonia"){
    col_number<-5
  }
 
  for (i in 1:length(state_names)){ 
    
    state_outcomes <- subset(outcomes, outcomes[2]==state_names[i])
    outcomes2<-order(state_outcomes[,col_number], state_outcomes[,1])
    state_outcomes=state_outcomes[outcomes2,]
    
    if(num=="worst"){
      num2<- sum(is.na(state_outcomes[col_number])==FALSE)
      }
    else{num2<-num
      }
    
    outcomes3 <- state_outcomes[num2,1]
    new_case<-c("hospital"=outcomes3, "state"=state_names[i])
    
    if(i==1){data_frame<-new_case}
    else {
      data_frame<-rbind(data_frame,new_case)
     }
  }
  
  data_frame<-as.data.frame(data_frame)
  return(data_frame)
  
}