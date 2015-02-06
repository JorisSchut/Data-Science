pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ##reads all the files and creates a data frame
  files_full <- list.files(directory, full.names=TRUE)
  dat<-data.frame()
  mean_pollutant<-numeric()
  
  ##Loops the requested files into the data frame
  for (i in id){
    dat <- rbind(dat, read.csv(files_full[i]))
  }
  
  #Calculates the mean of the desired pollutant
  if(pollutant=="sulfate"){
    append(mean_pollutant, mean(dat$sulfate, na.rm=TRUE)) -> mean_pollutant
  }
  
  if(pollutant=="nitrate"){
    append(mean_pollutant, mean(dat$nitrate, na.rm=TRUE)) -> mean_pollutant
  }
  
  ##Returns the mean of the pollutant
  return(mean_pollutant)
 
}  
  
  
  