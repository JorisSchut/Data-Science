download_data<-function(){
  ##Function that loads the data into a seperate file,
  ##sets your working directory and unzips it into 2 files.
  ##You only need to run this function if you do not have the data on your local machine.
  ##Warning: filesize = 59.9MB
  
  if(!file.exists("./data")){
    #Creates a new folder called "data" if you do not have one already.
    dir.create("./data")
  }
  
  #downloads the file
  fileUrl1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl1,destfile="./data/phonedata.zip") 
  #add method="curl" as an argument when running on Mac
  
  #unzips the downloaded file in the (new) data directory
  unzip("./data/phonedata.zip", exdir="./data")
}

loader<-function(){
  ##Function that loads the data into R
  ##Due to the large size of the test and training data this micht take a while
  library(data.table)
  
  ## Loads the data of the various sets into R
  trainingdata<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
  trainingdata<-as.data.table(trainingdata)
  #traininglabel<-read.table("./data/UCI HAR Dataset/train/y_train.txt", sep=" ", dec=".")
  #testdata<-read.table("./data/UCI HAR Dataset/test/X_test.txt", sep=" ", dec=".")
  #testlabel<-read.table("./data/UCI HAR Dataset/test/y_test.txt", sep=" ", dec=".")
}