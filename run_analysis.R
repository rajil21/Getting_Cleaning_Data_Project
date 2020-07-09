
# Downloading and unziping data folder

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file( url, destfile = "data_folder.zip" )
unzip("data_folder.zip")

# setting up working dir

setwd("./UCI HAR Dataset")

# extracting train and test files into corresponding trainfile and testfile respectively 

trainfile <- list.files( "train", full.names = TRUE )[-1]
testfile  <- list.files( "test" , full.names = TRUE )[-1]
  
# Making single file of them and reading as a table 

file <- c( trainfile, testfile )
data <- lapply( file, read.table, stringsAsFactors = FALSE, header = FALSE )


# TASK 1 : 
#          Merges the training and the test sets to create one data set
# rbind the train and test data by each variable

#  merging the train and test dataset with their corresponding names 

data1 <- mapply ( rbind, data[ c(1:3) ], data[ c(4:6) ] )

# Now column binding data1 to get a dataframe of 
# column 1 = subject, column 2~562 = feature,  column 563 = activity

data2 <- do.call( cbind, data1 )


# TASK 2 :
#         For the feature column, extracts only the measurements on the 
# mean and standard deviation for each measurement

# match it using features.txt (third file in list.file() in my working dir)

featurenames <- fread( list.files()[3], header = FALSE, stringsAsFactor = FALSE )

# Before going to TASK 2 we need to  set the column names for data2, 
# does the task required in 

# TASK 4 :
#         Appropriately labels the data set with descriptive variable names.

setnames( data2, c(1:563), c( "subject", featurenames$V2, "activity" ) )

# Extract only the column that have mean() or std() in the end
# Add 1 to it, bcz the first column in data2 is subject not feature
# Don't just use mean when doing matching, this will include meanFreq()
# Each backslash must be expressed as \\

measurements <- grep( "std|mean\\(\\)", featurenames$V2 ) + 1

# data3 : contains only the mean and standard deviation for feature column 

data3 <- data2[, c( 1, measurements, 563 ) ]


# TASK 3 :
#         Use descriptive activity names to name the activities in the data set

# match it using activity_labels.txt(first file in list.file() in my working dir)

activitynames <- fread( list.files()[1], header = FALSE, stringsAsFactor = FALSE )

data3$activity <- activitynames$V2[ match( data3$activity, activitynames$V1 ) ]


# TASK 5 : 
#          From the data set in step 4, creates a second, independent tidy data set, 
# with the average of each variable for each activity and each subject.

data4 <- aggregate( . ~ subject + activity, data = data3, FUN = mean )

# write out data4 to tidyData.txt

write.table( data4, "tidyData.txt", row.names = FALSE )


