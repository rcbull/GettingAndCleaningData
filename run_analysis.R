
## set working directory
setwd("G:/github/GettingAndCleaningData")

# folder with data
folder <- "UCI HAR Dataset"

# columns be read
# features data mean() and std()
filename <- sprintf("%s/features.txt", folder)
conn <- file(filename, "r")
idx <- grep("mean\\(\\)|std\\(\\)", readLines(conn))
close(conn)

# function read:
# this function to read files
make.read <- function(directory, index = idx){
  subject.df <- read.table(sprintf("%s/%s/subject_%s.txt",
                                   folder,directory, directory))
  y.df <- read.table(sprintf("%s/%s/y_%s.txt",
                             folder, directory, directory))
  X.df <- read.table(sprintf("%s/%s/X_%s.txt",
                             folder, directory, directory))[, index]
  dataset <- cbind(subject.df, y.df, X.df)
  
  # merge activity labels to sensor data
  activity.df <- read.table(sprintf("%s/activity_labels.txt", folder))
  suppressWarnings(dataset <- merge(dataset, activity.df, by.x = 2,
                                    by.y = 1))
  dataset[1] <- NULL
  
  dataset <- dataset[c(1, dim(dataset)[2], seq(2, dim(dataset)[2]-1))]
  dataset
}

# combined the train and test data frames
train.df <- make.read("train")
test.df <- make.read("test")
clean.df <- rbind(train.df, test.df)

# process features to be used as column names for the tidy dataset
hdr <- read.table(filename, row.names = NULL)[idx, 2]
hdr <- gsub("-", ".", hdr)
hdr <- gsub("\\(\\)", "", hdr)
hdr <- sub("^[t]", "Time", hdr)
hdr <- sub("^[f]", "Freq", hdr)
hdr <- sub("(Body)+", "Body", hdr)
colnames(clean.df) <- c("subject.id", "activity", hdr)

# ordering tidy dataset rows by subject then activity
clean.df <- clean.df[order(clean.df$subject.id, clean.df$activity), ]
row.names(clean.df) <- NULL

# uses the aggregate function to summarize
tidy.df <- with(clean.df,
                aggregate(clean.df[c(seq(3, dim(clean.df)[2]))],
                          by = list(subject.id, activity),
                          mean))
colnames(tidy.df) [1]<- "subject"
colnames(tidy.df)[2] <- "activity"

# saves the aggregated dataset for reporting
write.table(tidy.df, file = "tidy.txt", row.names = FALSE, , quote = FALSE)
