# This script merges the text file with the PDS categories to the existing data.
# Because we have already created the two data files, I am passing in the exploratory data set.
# This step should be moved earlier, in the first script (the merge script), in which case you should pass in the whole data set.

library(tidyr)
library(dplyr)
library(here)

data_dir = ((dirname(here()))) 

#phase_folder = "exploratory"  # select the appropiate folder.
phase_folder = "confirmatory"  # select the appropiate folder.

data_folder <- file.path(data_dir,"ABCD","derivatives",phase_folder)

#file_name <- "nda20_exploratory.csv" # specify file name here. 
file_name <- "nda20_confirmatory.csv" # specify file name here. 

fulldata <- read.csv(file.path(data_folder,file_name))
nrow(fulldata) # exploratory = 5934; confirmatory = 5935.

# Change this to your directory and the name of your already created data file.
#datafile <- "/Users/nataliesaragosa-harris/Desktop/ABCD/output/nda20_exploratory.csv"

# This should be your directory where you have the abcd_ssphp01 file saved.
textfile <- "/Users/nataliesaragosa-harris/Desktop/ABCD/individualdatafiles/abcd_ssphp01.txt"
abcd_ssphp01 <- read.delim(textfile, header = TRUE) 

# Save as a csv file (if you have not already).
# csvfile <- "/Users/nataliesaragosa-harris/Desktop/ABCD/individualdatafiles/abcd_ssphp01.csv" # Name of file to save.
# write.table(abcd_ssphp01, file=csvfile, sep=",", col.names=TRUE, row.names=FALSE)

abcd_ssphp01 <- abcd_ssphp01[-1,]

# Remove columns that exist in data already.
abcd_ssphp01 <- abcd_ssphp01[,!(names(abcd_ssphp01) %in% c("collection_id", "collection_title", "subjectkey","interview_age", "interview_date","sex"))]
colnames(abcd_ssphp01)

# Merge by participant ID and eventname.
data <- merge(fulldata, abcd_ssphp01, by = c("src_subject_id","eventname"))

# Create new variable that combines male and female columns.
data <- data %>%
  mutate(
    pds_p_ss_category = ifelse((sex=="F"), pds_p_ss_female_category, pds_p_ss_male_category),
    pds_p_ss_cat_nm = ifelse((sex=="F"), pds_p_ss_female_cat_nm, pds_p_ss_male_cat_nm),
    pds_p_ss_cat_nt = ifelse((sex=="F"), pds_p_ss_female_cat_nt, pds_p_ss_male_cat_nt))

# pds <- data[,c("src_subject_id","PDS_sum","PDS_score", 
#        "pds_p_ss_female_category", "pds_p_ss_male_category", "pds_p_ss_category","pds_p_ss_cat_nm","pds_p_ss_cat_nt")]


write.csv(data,file.path(data_folder,file_name), row.names = FALSE)
