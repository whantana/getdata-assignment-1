#
# 
#  The run_analysis script . According to requirements it does the following
#
#  #1. Merges the training and the test sets to create one data set.
#  #2. Extracts only the measurements on the mean and standard deviation 
#       for each measurement. 
#  #3. Uses descriptive activity names to name the activities in the data set
#  #4. Appropriately labels the data set with descriptive variable names. 
#  #5. From the data set in step 4, creates a second, independent tidy data set
#      with the average of each variable for each activity and each subject.
#
#

library(dplyr)

run_analysis <- function() {
    
    # I keep my datasets in the $LOCAL_DATASETS path.
    # You can put them near the code "./data/" .
    base_path <- paste(Sys.getenv("LOCAL_DATASETS"),"uci_har_dataset",sep="/")
    base_path <- if (!file.exists(base_path)) "./data" else base_path
    
    if(!file.exists(base_path))
        stop("Dataset directory \"./data\" not found! Stopping now.")
    message(sprintf("UCI HAR Dataset found at \"%s\".",base_path))
    
    # load and setup training & test datasets
    TR_DF <- load_setup_dataset(base_path,"train")
    TE_DF <- load_setup_dataset(base_path,"test")
    message("Train/Test Datasets are now setup and ready for merge.")
    
    # merge datasets
    # does requirement #1
    DF <- rbind(TR_DF,TE_DF)
    message("Train/Test Datasets are now merged to a single data frame.")
    

    GDF <- create_grouped_summarization(DF)
    message("Grouped (subject_id,activity) and summarized data frame is now ready.")
    
    # returning both data frames
    return(list(DF,GDF))
}


#  meets requirement #3
load_and_relabel_activities <- function(base_path,Y) {
    activities_path <- paste(base_path, "activity_labels.txt" , sep ="/")
    A <- read.table(activities_path)
    mYA <- merge(Y,A,by="V1")
    mYA$V1 <- mYA$V2
    mYA$V2 <- NULL
    names(mYA) <- c("activity")
    mYA
}


#  meets requirement #4
load_and_relabel_features <- function(base_path,X) {
    features_path <- paste(base_path, "features.txt" , sep ="/")
    F <- read.table(features_path)
    names(X) <- make.unique(sapply(F$V2,function(x) {
        x <- gsub("\\(\\)","",x)
        x <- gsub("\\(|\\)",".",x)
        x <- gsub("\\,",".",x)
        x <- gsub("\\-","_",x)
    }, USE.NAMES=FALSE))
    X
}


#  meets requirement #2
extract_features_of_interest <- function(X) {
    X <- select(X, contains("_mean_"),contains("_std_"))   
}


#  meets requirements #1,#2,#3,#4
load_setup_dataset <- function(base_path,sub_path) {
    
    message(sprintf("\tLoad and setup of \"%s/%s\".",base_path,sub_path))
    
    # if dataset does not exist stop
    filenames <- sprintf(c("X_%s.txt","y_%s.txt","subject_%s.txt"),sub_path)
    dataset_paths <- sapply(filenames,
                            function(x) { paste(base_path,sub_path,x,sep ="/")},
                            USE.NAMES = FALSE)
    if (!all(file.exists(dataset_paths))) 
        stop("Dataset not found or partially found! Stopping Now.")
    
    # read data and store them in data frames
    
    # load feature values and relabel with the feature names.
    # extract columns that have either "mean() or std()" in ther name.
    X <- read.table(dataset_paths[1], strip.white=TRUE, header=FALSE)
    X <- load_and_relabel_features(base_path,X)
    X <- extract_features_of_interest(X)
   
    # load activity values and relabel them
    Y <- read.table(dataset_paths[2], strip.white=TRUE, header=FALSE)
    Y <- load_and_relabel_activities(base_path,Y)
    
    # load the subjects
    S <- read.table(dataset_paths[3], strip.white=TRUE, header=FALSE)
    names(S) <- c("subject_id")
        
    # bind the above as columns in a data.frame
    SYX <- cbind(S,Y,X)
}

#  meets requirements #5
create_grouped_summarization <- function(DF) {
    # group by subject id and activity and summarise each column using mean
    GDF <- group_by(DF,subject_id,activity) %>% summarise_each(funs(mean))
    filename <- "result.txt"
    write.table(GDF,filename,row.names=FALSE)
    message(sprintf("\tWriting output to filename : %s",filename))
    GDF
}