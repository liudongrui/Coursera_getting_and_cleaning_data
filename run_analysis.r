library(data.table)
library(reshape2)
## Downloading the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,file.path(getwd(),"Dataset.zip"))
## Unzip data
## Read the data into R
tem1<-read.table("UCI HAR Dataset/train/X_train.txt")
tem2<-read.table("UCI HAR Dataset/test/X_test.txt")
A<-rbind(tem1,tem2)

tem3<-read.table("UCI HAR Dataset/train/subject_train.txt")
tem4<-read.table("UCI HAR Dataset/test/subject_test.txt")
B<-rbind(tem3,tem4)

tem5 <- read.table("UCI HAR Dataset/train/y_train.txt")
tem6 <- read.table("UCI HAR Dataset/test/y_test.txt")
C <- rbind(tem5, tem6)
## looking for good features
features <- read.table("UCI HAR Dataset/features.txt")
good<-grep("-mean\\(\\)|-std\\(\\)", features[, 2])
A<-A[, good]
names(A) <- features[good, 2]
names(A) <- gsub("\\(|\\)", "", names(A))
names(A) <- tolower(names(A))

label<- read.table("UCI HAR Dataset/activity_labels.txt")
label[, 2] = gsub("_", "", tolower(as.character(label[, 2])))
C[,1] = label[C[,1], 2]
names(C) <- "activity"

names(B)<-"subject"
clean<-cbind(B, C, A)

uniqueSubjects <- unique(B)[,1]
numSubjects<- length(unique(B)[,1])
numActivities <- length(label[,1])
numCols <- dim(clean)[2]
result <- clean[1:(numSubjects*numActivities), ]

row <- 1
for (n in 1:numSubjects) {
    for (m in 1:numActivities) {
        result[row, 1] <- uniqueSubjects[n]
        result[row, 2] <-label[m, 2]
        tem = clean[clean$subject==n & clean$activity==label[m, 2], ]
        result[row, 3:numCols] <- colMeans(tem[, 3:numCols])
        row = row+1
    }
}
write.table(clean, "merged.txt",row.names = FALSE)
write.table(result, "averages.txt",row.name = FALSE)
