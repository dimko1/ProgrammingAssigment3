library(data.table)

# function used to download file and extract it from the network
downloadFile <- function() {
  if (!file.exists('data.zip')) {
    download.file('http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
                  dest="data.zip"
                  )
  }
}

# unzipping data for file
unzipFile <- function() {
  if (!file.exists('data.zip')) {
    stop("File with data is not downloaded")
  }
  unzip('data.zip', exdir = ".")
}

analizeData <- function() {
  # reading test data
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  sbj_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  # reading training data
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  sbj_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  # reading features and activities
  features <- read.table("UCI HAR Dataset/features.txt")
  activities <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  # merging train and test data
  X_data <- rbind(x_test, x_train)
  Y_data <- rbind(y_test, y_train)
  sbj <- rbind(sbj_test, sbj_train)
  
  # extracting data related only for mean values and standard deviation
  index <- grep("mean\\(\\)|std\\(\\)", features[,2])
  X_data <- X_data[,index]
  
  # extracting correct names for activities
  Y_data[,1] <- activities[Y_data[,1],2]
  
  
  # var names
  names <- features[index, 2] 
  
  names(X_data) <- names
  names(sbj) <- "SubjectID"
  names(Y_data) <- "Activity"
  
  head(cbind(sbj, Y_data, X_data))
  pure_data <- data.table(cbind(sbj, Y_data, X_data))
  head(pure_data)
  
  # generating tidy data dataset
  tidy_data <- pure_data[, lapply(.SD, mean), by = 'SubjectID,Activity']
  
  
  write.table(tidy_data, file="rs.txt", col.names = F)
}

