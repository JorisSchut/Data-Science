complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  ##reads all the files and creates a data frame to strore the cases in
  files_full <- list.files(directory, full.names=TRUE)
  cases<-data.frame(ncol=2)
   
  
  ##Loops the requested files into the data frame
  for (i in id){
  
    #tracks the complete cases
    complete <- sum(complete.cases(read.csv(files_full[i])))
    
    #binds the cases together
    cases <- rbind(cases, c(i, complete))
  }
  
  #Adds headers to the output
  names(cases)<- c("id","nobs")
  
  return(cases)
}