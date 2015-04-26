
## Create a Directory if it doesnt exist and download file; 
## Else load files from local directory "temp" Team and Store file names in object ~ Data
if(!file.exists("./temp")){
      dir.create("./temp")
      Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(Url,destfile="./temp/Dataset.zip",mode="wb")
      Data <- unzip("./temp/Dataset.zip",exdir = "./temp",unzip = "internal")
      unlink("./temp")
}else{
      Data <- paste0("./temp/UCI HAR Dataset/",
                     list.files("./temp/UCI HAR Dataset",recursive = TRUE))
}

## Reduce number of file to the required files and read in files.
DataToRead <- Data[nchar(Data)<50&!grepl("info|README",Data)]
ListFiles <- sapply(DataToRead,function(x)as.data.frame(read.table(x)),USE.NAMES = TRUE)
File.index <- names(ListFiles)## Use to check the list for reference of each element

## Combine data sets using data stored in ListFiles.
## Provide descriptive variable names to Columns
## Retain only data relating to mean and std.
Features <- rbind(ListFiles[[4]],ListFiles[[7]])
SubjActiv <- cbind((rbind(ListFiles[[5]],ListFiles[[8]])),
                   (rbind(ListFiles[[3]],ListFiles[[6]])))

colnames(Features) <- ListFiles[[2]][,2]
Features <- Features[,grepl("mean|std",colnames(Features),ignore.case = TRUE)]

colnames(SubjActiv) <- c("Activity","Subject")

## Combine Data sets and Lable Activities with descriptive activity names
Combined <- cbind(SubjActiv,Features)
Combined$Activity <-  ListFiles[[1]][,2][match(Combined$Activity,ListFiles[[1]][,1])]
Combined$Subject <- as.factor(Combined$Subject)

## Create a Tidy Data Set
library(reshape)
Tidy <- melt(Combined,id = c("Activity","Subject"))

## Summarise by average of each variable for each activity and each subject.
library(dplyr)
Tidy2 <- group_by(Tidy,Subject,Activity,variable)%>%summarise_each(funs(mean))
write.table(Tidy2,file="Tidy_part5.txt",row.names = FALSE)
