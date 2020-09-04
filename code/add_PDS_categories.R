# This script merges the text file with the PDS categories to the existing data.
# Because we have already created the two data files, I am passing in the exploratory data set.
# This step should be moved earlier, in the first script (the merge script), in which case you should pass in the whole data set.

library(tidyr)
library(dplyr)

# Change this to your directory and the name of your already created data file.
datafile <- "/Users/nataliesaragosa-harris/Desktop/ABCD/output/nda20_exploratory.csv"
# Change this to your directory where you have the abcd_ssphp01 file saved.
textfile <- "/Users/nataliesaragosa-harris/Desktop/ABCD/data/abcd_ssphp01.txt"
csvfile <- "/Users/nataliesaragosa-harris/Desktop/ABCD/data/abcd_ssphp01.csv" # Will create this file in this script.

data <- read.table(datafile, header=T, sep=",",as.is=TRUE,strip.white=TRUE,fill=TRUE)
length(data$src_subject_id) # 5934.

# Save as a csv file.
abcd_ssphp01 <- read.delim(textfile, header = TRUE) 
write.table(abcd_ssphp01, file=csvfile, sep=",", col.names=TRUE, row.names=FALSE)

abcd_ssphp01 <- abcd_ssphp01[-1,]

# Remove columns that exist in data already.
abcd_ssphp01 <- abcd_ssphp01[,!(names(abcd_ssphp01) %in% c("collection_id", "collection_title", "subjectkey","interview_age", "interview_date","sex"))]
colnames(abcd_ssphp01)

# Merge by participant ID and eventname.
data <- merge(data, abcd_ssphp01, by = c("src_subject_id","eventname"))

# Create new variable that combines male and female columns.
data <- data %>%
  mutate(
    pds_p_ss_category = ifelse((sex=="F"), pds_p_ss_female_category, pds_p_ss_male_category),
    pds_p_ss_cat_nm = ifelse((sex=="F"), pds_p_ss_female_cat_nm, pds_p_ss_male_cat_nm),
    pds_p_ss_cat_nt = ifelse((sex=="F"), pds_p_ss_female_cat_nt, pds_p_ss_male_cat_nt))

# pds <- data[,c("src_subject_id","PDS_sum","PDS_score", 
#        "pds_p_ss_female_category", "pds_p_ss_male_category", "pds_p_ss_category","pds_p_ss_cat_nm","pds_p_ss_cat_nt")]

write.csv(data,datafile, row.names = FALSE)
