run_analysis <- function(){

       

# 1.      

# Reading data files from both folders

        test_set<- read.table(file ="UCI HAR Dataset/test/X_test.txt")

        train_set<-read.table(file ="UCI HAR Dataset/train/X_train.txt")

       

# Merging now these files and then storing into merge_set

        merge_set<-rbind(test_set,train_set)

       

# Reading label file from both folders

        test_labels<- read.table(file ="UCI HAR Dataset/test/y_test.txt")

        train_labels<-read.table(file ="UCI HAR Dataset/train/y_train.txt")

 

# Merging now these files and then storing into merge_labels               

        merge_labels<-rbind(test_labels,train_labels)

 

# Reading subjects file from both folders

        test_subjects<- read.table(file ="UCI HAR Dataset/test/subject_test.txt")

        train_subjects<-read.table(file ="UCI HAR Dataset/train/subject_train.txt")

       

# Merging now these files and then storing into merge_subjects 

        merge_subjects<-rbind(test_subjects,train_subjects)

 

# Reading features and activity labels

        features <- read.table("UCI HAR Dataset/features.txt")

        activity_labels <- read.table(file ="UCI HAR Dataset/activity_labels.txt")

 

# selecting collumns for mean and std

        mean_l <- (mapply(grepl,"mean\\(",features['V2']))

        std_l <- mapply(grepl,"std\\(",features['V2'])

        mean_std_l <- mean_l | std_l

 

        colnames_we_want<- filter(select(features, V2),grepl("mean\\(",V2) | grepl("std\\(",V2))

# Selecting and filtering data from merge set with help of collumn we want

        data <- merge_set[,mean_std_l]

 

# Naming the collumns of data

        colnames(data) <- colnames_we_want[,1]

        data<- tbl_df(data)

 

# naming the activity corresponding their number given in data

        activity_names<-mapply(function(x,y){

                return (y[x])

        },merge_labels[,1],activity_labels['V2'])

       

 

        data <- mutate(subject=merge_subjects[,1],activity=activity_names,data)

        num_col <- ncol(data) - 2

        ans <- aggregate(data[,1:num_col], by=list(subject = data$subject, activity =data$activity),mean)

       

        write.table(ans,"data_with_means.txt",row.names=FALSE)

}
