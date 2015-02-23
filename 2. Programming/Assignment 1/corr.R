corr <- function(directory, threshold = 0) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ##reads all the files 
  files_full <- list.files(directory, full.names=TRUE)
  correlation <- numeric(0)
  
  for (i in 1:length(files_full)){
    complete <- sum(complete.cases(read.csv(files_full[i])))
   
    if(complete>threshold){
      dat <- read.csv(files_full[i])
      correlation<-append(correlation, cor(dat$sulfate, dat$nitrate, use="complete.obs"))
    }
  
    else{
      
      next()
        }
  }
 return(correlation)
}