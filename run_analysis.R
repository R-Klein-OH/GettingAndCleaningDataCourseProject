###Set Working Directory###

setwd("F:\\Personal\\Coursera\\getting_and_cleaning_data_course\\course_project")

###Load Packages###

library(tidyr)
library(dplyr)

###Load and Prepare Variable Names###

raw_data_root_path <- "F:\\Personal\\Coursera\\getting_and_cleaning_data_course\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\"

features_list <- read.table(paste0(raw_data_root_path, 'features.txt'), header=FALSE, stringsAsFactors=FALSE)
#str(features_list)
names(features_list) <- c('variable_number', 'variable_name')
#str(features_list)

#View(features_list)

###Subset of Desired Variable Names###

#79 matches when do not exclude meanFreq()
#66 matches when exclude meanFreq()
mean_sd_features_list <- features_list[(grepl("mean()", features_list$variable_name) | grepl("std()", features_list$variable_name)) 
                                            & !(grepl("meanFreq()", features_list$variable_name)), ]
#View(mean_sd_features_list)

###Load Activity Labels###

activity_labels_list <- read.table(paste0(raw_data_root_path, 'activity_labels.txt'), header=FALSE, stringsAsFactors=FALSE)
#str(activity_labels_list)
names(activity_labels_list) <- c('activity_label_number', 'activity_label_name')
#str(activity_labels_list)


###Test Data###

test_root_path <- "F:\\Personal\\Coursera\\getting_and_cleaning_data_course\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\"
#test_inertial_signals_path <- "F:\\Personal\\Coursera\\getting_and_cleaning_data_course\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\Inertial Signals\\"
#body_acc
#body_gyro
#total_acc


test_subject <- read.table(paste0(test_root_path, 'subject_test.txt'), header=FALSE)
names(test_subject) <- 'subject_id'

x_test <- read.table(paste0(test_root_path, 'X_test.txt'), header=FALSE)
x_test_sub <- x_test[ , mean_sd_features_list$variable_number]
rm("x_test")
names(x_test_sub) <- mean_sd_features_list$variable_name


y_test <- read.table(paste0(test_root_path, 'y_test.txt'), header=FALSE)
names(y_test) <- 'activity_label'
y_test$activity_label <- as.factor(y_test$activity_label)
#activity_labels_list$activity_label_name
levels(y_test$activity_label)[levels(y_test$activity_label)=="1"] <- "WALKING"
levels(y_test$activity_label)[levels(y_test$activity_label)=="2"] <- "WALKING_UPSTAIRS"
levels(y_test$activity_label)[levels(y_test$activity_label)=="3"] <- "WALKING_DOWNSTAIRS"
levels(y_test$activity_label)[levels(y_test$activity_label)=="4"] <- "SITTING"
levels(y_test$activity_label)[levels(y_test$activity_label)=="5"] <- "STANDING"
levels(y_test$activity_label)[levels(y_test$activity_label)=="6"] <- "LAYING"
#table(y_test$activity_label)

#dim(x_test_sub)

#2,947 rows each
#nrow(test_subject)
#nrow(x_test_sub)
#nrow(y_test)


#View(head(x_test))

###Train Data###

train_root_path <- "F:\\Personal\\Coursera\\getting_and_cleaning_data_course\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\"
#train_inertial_signals_path <- "F:\\Personal\\Coursera\\getting_and_cleaning_data_course\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\Inertial Signals\\"

train_subject <- read.table(paste0(train_root_path, 'subject_train.txt'), header=FALSE)
names(train_subject) <- 'subject_id'

x_train <- read.table(paste0(train_root_path, 'X_train.txt'), header=FALSE)
x_train_sub <- x_train[ , mean_sd_features_list$variable_number]
rm("x_train")
names(x_train_sub) <- mean_sd_features_list$variable_name

y_train <- read.table(paste0(train_root_path, 'y_train.txt'), header=FALSE)
names(y_train) <- 'activity_label'
y_train$activity_label <- as.factor(y_train$activity_label)
#activity_labels_list$activity_label_name
levels(y_train$activity_label)[levels(y_train$activity_label)=="1"] <- "WALKING"
levels(y_train$activity_label)[levels(y_train$activity_label)=="2"] <- "WALKING_UPSTAIRS"
levels(y_train$activity_label)[levels(y_train$activity_label)=="3"] <- "WALKING_DOWNSTAIRS"
levels(y_train$activity_label)[levels(y_train$activity_label)=="4"] <- "SITTING"
levels(y_train$activity_label)[levels(y_train$activity_label)=="5"] <- "STANDING"
levels(y_train$activity_label)[levels(y_train$activity_label)=="6"] <- "LAYING"


#7,352 rows each
#nrow(train_subject)
#nrow(x_train_sub)
#nrow(y_train)

#561
#length(names(x_train))


###Column Bind Each Group###

test_wide_raw <- cbind(test_subject, x_test_sub, y_test)
train_wide_raw <- cbind(train_subject, x_train_sub, y_train)

#dim(test_wide_raw)
#dim(train_wide_raw)

rm("test_subject", "x_test_sub", "y_test", "train_subject", "x_train_sub", "y_train")


###Stack Test and Train###

combined_wide_raw <- rbind(test_wide_raw, train_wide_raw)
rm("test_wide_raw", "train_wide_raw")

###Transpose from Wide to Long###

long_tmp <- gather(combined_wide_raw, variable_name, variable_value, mean_sd_features_list$variable_name)
long <- separate(data=long_tmp, col = variable_name, into = c("measure", "summary_function", "coordinate"))
rm("long_tmp")

###Calculate Average Measure Value by Defined Group###

long_by_group <- group_by(long, subject_id, activity_label, measure, summary_function, coordinate)
combined_long_summary <- summarize(long_by_group, avg=mean(variable_value))
head(combined_long_summary)

###Save Tidy Summarized Dataset###

write.csv(combined_long_summary, "combined_long_summary.csv", row.names=FALSE)
