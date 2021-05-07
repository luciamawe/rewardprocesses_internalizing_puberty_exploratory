require(dplyr)


data_dir = ((dirname(here()))) #figure out how to navigate to data source
data_folder <- file.path(data_dir,"ABCD","derivatives","exploratory")  
file_name <- "nda30_exploratory.csv" #specify file name here 
output_directory <- data_folder

nda30 <- read.table(file.path(data_folder,file_name),header=T, sep=",",as.is=TRUE,strip.white=TRUE,fill=TRUE)

data <- nda30
data <- data %>% filter(eventname=="baseline_year_1_arm_1")

##Check to see if 'sex' and 'hormone_sal_sex' data matches.
###Exploratory
#5 Female with misclassified Male tubes. 6 Male with misclassified Female tubes. 49 either had issues at saliva collection or had NA gender values.
#Let's get rid of them. We go down from 5691 -> 5905 (-56)

table(data$sex, data$hormone_sal_sex)

data  <- data[-c(which(data$sex == "M" & data$hormone_sal_sex == 1), 
             which(data$sex =="F" & data$hormone_hormone_sal_sex == 2),
             which(data$hormone_sal_sex == 3),
             which(data$hormone_sal_sex == 4),
             which(data$hormone_sal_sex == 5),
             which(is.na(data$sex)),
             which(is.na(data$hormone_sal_sex))),]


###################### NDA --- DEAP
# hormone_sal_sex       1       Pink (female)
# hormone_sal_sex       2       Blue (male)
# hormone_sal_sex       3       Participant unable to complete
# hormone_sal_sex       4       Participant/Parent refused
# hormone_sal_sex       5       Not collected (other)      

data$hormon_sal_notes_y___1

#Let's filter the data
#The filter scheme is to check records for any RA saliva collection notes. If true, then flag the record. 
#Then check flagged records and see if the Salimetrics value is out of range per hormone.
#If yes, then change value to NA, else keep the existing values for each replicate.
#Finally, average the two replicates into a new field.
data$hormone_notes_ss <- as.numeric(data$hormon_sal_notes_y___2) + 
                       as.numeric(data$hormon_sal_notes_y___3) +
                       as.numeric(data$hormon_sal_notes_y___4) + 
                       as.numeric(data$hormon_sal_notes_y___5) + 
                       as.numeric(data$hormon_sal_notes_y___6)
rownums <- which(data$hormone_notes_ss > 1)

#DHEA
data$filtered_dhea <- NA
data$filtered_dhea_rep1 <- as.numeric(data$hormone_scr_dhea_rep1)
data$filtered_dhea_rep2 <- data$hormone_scr_dhea_rep2
data$filtered_dhea[which(data$hormone_scr_dhea_rep1_nd == 1)] <- 0
data$filtered_dhea[which(data$hormone_scr_dhea_rep2_nd == 1)] <- 0
rownums_rep1 <- which(data$hormone_scr_dhea_rep1 < 5 | data$hormone_scr_dhea_rep1 > 1000)
rownums_rep2 <- which(data$hormone_scr_dhea_rep2 < 5 | data$hormone_scr_dhea_rep2 > 1000)
data$filtered_dhea_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA
data$filtered_dhea_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA
data$filtered_dhea <- apply(data[, c("filtered_dhea_rep1", "filtered_dhea_rep2")], 1, function(x) mean(x, na.rm=T))

#Testosterone
data$filtered_testosterone <- NA
data$filtered_testosterone_rep1 <- data$hormone_scr_ert_rep1
data$filtered_testosterone_rep2 <- data$hormone_scr_ert_rep2
data$filtered_testosterone[which(data$hormone_scr_ert_rep1_nd == 1)] <- 0
data$filtered_testosterone[which(data$hormone_scr_ert_rep2_nd == 1)] <- 0
rownums_rep1 <- which(data$hormone_scr_ert_rep1 < 1 | data$hormone_scr_ert_rep1 > 600)
rownums_rep2 <- which(data$hormone_scr_ert_rep2 < 1 | data$hormone_scr_ert_rep2 > 600)
data$filtered_testosterone_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA
data$filtered_testosterone_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA
data$filtered_testosterone <- apply(data[, c("filtered_testosterone_rep1", "filtered_testosterone_rep2")], 1, function(x) mean(x, na.rm=T))

#Estradiol
data$filtered_estradiol <- NA
data$filtered_estradiol_rep1 <- data$hormone_scr_hse_rep1
data$filtered_estradiol_rep2 <- data$hormone_scr_hse_rep2
data$filtered_estradiol[which(data$hormone_scr_hse_rep1_nd == 1)] <- 0
data$filtered_estradiol[which(data$hormone_scr_hse_rep2_nd == 1)] <- 0
rownums_rep1 <- which(data$hormone_scr_hse_rep1 < 0.1 | data$hormone_scr_hse_rep1 > 32)
rownums_rep2 <- which(data$hormone_scr_hse_rep2 < 0.1 | data$hormone_scr_hse_rep2 > 32)
data$filtered_estradiol_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA
data$filtered_estradiol_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA
data$filtered_estradiol <- apply(data[, c("filtered_estradiol_rep1", "filtered_estradiol_rep2")], 1, function(x) mean(x, na.rm=T))

#Filter
data$filtered_testosterone_z <- scale(data$filtered_testosterone)
test_outliers <- subset(data, filtered_testosterone_z > -3 & filtered_testosterone_z < 3)

data$filtered_estradiol_z <- scale(data$filtered_estradiol)
estradiol_outliers <- subset(data, filtered_estradiol_z > -3 & filtered_estradiol_z < 3)

data$filtered_dhea_z <- scale(data$filtered_dhea)
dhea_outliers <- subset(data, filtered_dhea_z > -3 & filtered_dhea_z < 3)

#Explore Testosterone
boxplot(data$filtered_testosterone)
boxplot(data$hormone_scr_ert_mean)

boxplot(data$hormone_sal_sex,data$filtered_testosterone, ylim = c(0,500))
boxplot(data$hormone_scr_ert_mean~data$sex,ylim = c(0,50))

data$interview_age_z <- scale(data$interview_age)

hist(data$filtered_testosterone)
hist(test_outliers$filtered_testosterone)

boxplot(data$filtered_testosterone)
boxplot(test_outliers$filtered_testosterone)

boxplot(data$filtered_testosterone_z)

boxplot(test_outliers$filtered_testosterone~test_outliers$sex)
boxplot(data$filtered_testosterone~data$sex,ylim = c(0,600))

summary(data$filtered_testosterone)
summary(data$hormone_scr_ert_mean)


## This code has been copied over to define_dataframes on May 7, 2021... This code should not be run. 