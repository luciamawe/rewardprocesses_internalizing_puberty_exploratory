# Compare anyone who was removed as an outlier (for any variable, in any model) to the rest of the group on demographic variables.

library(ggplot2)
library(here)
library(glue)
library(dplyr)
library(compareGroups)

data_dir = ((dirname(here()))) 

# First, let's load the exploratory data and assign unique names to the dataframes we will use for comparing groups from the exploratory data.
# When call the define_dataframes script, will keep only the baseline data.

phase_folder = "exploratory"
data_folder <- file.path(data_dir,"ABCD","derivatives",phase_folder)
file_name <- glue("nda30_{phase_folder}.csv")
fulldata <- read.csv(file.path(data_folder,file_name))

# Call function to define all of the dataframes.
script_path <- file.path(data_dir,"rewardprocesses_internalizing_puberty_exploratory","code","define_dataframes.R")
source(script_path)
nrow(data) # 5889.
nrow(subset(data, pds_p_ss_category == "Post")) # 7.

# Use PDS correct for all non-imaging variables.
# Use MID correct for all imaging variables.
fulldata_sample1 <- fulldata
nrow(fulldata_sample1) # 27321.

MID_task_correct_sample1 <- MID_task_correct
nrow(MID_task_correct_sample1) # 4617.
#length(unique(MID_task_correct_sample1$src_subject_id)) # 4610. So some people have two values?
#mid_sample1_duplicates <- MID_task_correct_sample1[duplicated(MID_task_correct_sample1$src_subject_id), ]
#View(mid_sample1_duplicates) # NDAR_INV3E0WVH3G has seven values.
MID_task_correct_sample1 <- MID_task_correct_sample1[!duplicated(MID_task_correct_sample1$src_subject_id),]
nrow(MID_task_correct_sample1) # 4610.

PDS_correct_sample1 <- PDS_correct
nrow(PDS_correct_sample1) # 5631.
#length(unique(PDS_correct_sample1$src_subject_id)) # 5624. So some people have two values?
#pds_sample1_duplicates <- PDS_correct_sample1[duplicated(PDS_correct_sample1$src_subject_id), ]
#View(pds_sample1_duplicates) # NDAR_INV3E0WVH3G has seven values.
PDS_correct_sample1 <- PDS_correct_sample1[!duplicated(PDS_correct_sample1$src_subject_id),]
nrow(PDS_correct_sample1) # 5624.

MID_task_correct_sample1 <- MID_task_correct_sample1 %>% dplyr::select("src_subject_id","rt_diff_large_neutral_z","rt_diff_large_small_z") 
sample1 <- merge(PDS_correct_sample1,MID_task_correct_sample1, by = "src_subject_id",all.x=TRUE, all.y = TRUE)
nrow(sample1) # 5624.

# Do the same process, but with the confirmatory sample.
phase_folder = "confirmatory"  # select the appropriate folder.
data_folder <- file.path(data_dir,"ABCD","derivatives",phase_folder)
file_name <- glue("nda30_{phase_folder}.csv")
fulldata <- read.csv(file.path(data_folder,file_name))
nrow(fulldata) #  27331 (confirmatory).

# Call function to define all of the dataframes.
script_path <- file.path(data_dir,"rewardprocesses_internalizing_puberty_exploratory","code","define_dataframes.R")
source(script_path)
nrow(data) # 5902.
nrow(subset(data, pds_p_ss_category == "Post")) # 3.

fulldata_sample2 <- fulldata
nrow(fulldata_sample2) # 27331
MID_task_correct_sample2 <- MID_task_correct
nrow(MID_task_correct_sample2) # 4648.
PDS_correct_sample2 <- PDS_correct
nrow(PDS_correct_sample2) # 5612.

MID_task_correct_sample2 <- MID_task_correct_sample2[!duplicated(MID_task_correct_sample2$src_subject_id),]
nrow(MID_task_correct_sample2) # 4634.
PDS_correct_sample2 <- PDS_correct_sample2[!duplicated(PDS_correct_sample2$src_subject_id),]
nrow(PDS_correct_sample2) # 5598.

MID_task_correct_sample2 <- MID_task_correct_sample2 %>% dplyr::select("src_subject_id","rt_diff_large_neutral_z","rt_diff_large_small_z") 
sample2 <- merge(PDS_correct_sample2,MID_task_correct_sample2, by = "src_subject_id",all.x=TRUE, all.y = TRUE)
nrow(sample2)  # 5598.

# List of variables in which we exclude outliers.
# BAS-RR.
# All neuroimaging data.
# MID reaction time data.
# Testosterone scores.

# Demographic variables to compare.
demographic_variables <- c("sex",
                           "bmi",
                           "interview_age",
                           "race.ethnicity.5level",
                           "household.income",
                           "high.educ",
                           "demo_race_hispanic")

variables_with_outliers <- c("hormone_scr_ert_mean_z",
                             "bisbas_ss_basm_rr_z",
                             "rt_diff_large_neutral_z",
                             "rt_diff_large_small_z",
                             "accumbens_rvsn_ant_z", # Accumbens reward vs. neutral anticipation.
                             "caudate_rvsn_ant_z", # Caudate reward vs. neutral anticipation.
                             "putamen_rvsn_ant_z", # Putamen reward vs. neutral anticipation.
                             "mOFC_rvsn_ant_z", # Medial OFC reward vs. neutral anticipation.
                             "lOFC_rvsn_ant_z", # Lateral OFC reward vs. neutral anticipation.
                             "accumbens_posvsneg_feedback_z", # Accumbens reward positive versus negative feedback.
                             "caudate_posvsneg_feedback_z", # Caudate reward positive versus negative feedback.
                             "putamen_posvsneg_feedback_z", # Putamen reward positive versus negative feedback.
                             "mOFC_posvsneg_feedback_z",  # Medial OFC reward positive versus negative feedback.
                             "lOFC_posvsneg_feedback_z") # Lateral OFC reward positive versus negative feedback.

sample1$sample = "sample1"
sample2$sample = "sample2"
both_samples <- rbind(sample1,sample2)
both_samples <- both_samples %>% select("src_subject_id","sample",demographic_variables,variables_with_outliers)
nrow(both_samples) # 11222.
both_samples$sample <- as.factor(both_samples$sample)
both_samples %>% count(sample)
# 
# is_outlier <- function(x){
#   return(x <  -3 | x > 3)
# }

# 
# penguins %>% 
#   filter(!is.na(bill_length_mm)) %>% 
#   mutate(
#     category = case_when(
#       if_all(contains("bill"), big) ~ "both big", 
#       if_any(contains("bill"), big) ~ "one big", 
#       TRUE                          ~ "small"
#     )) %>% 
#   count(category)
# 
# both_samples <- both_samples %>%
#   mutate(is_outlier_any = case_when(
#     if_any(variables_with_outliers, is_outlier) ~ "yes",
#     TRUE  ~ "no"))

# None of those are working so have to do this instead.

both_samples <- both_samples %>%
  mutate(is_outlier_any = case_when(
                  (hormone_scr_ert_mean_z < -3 |  hormone_scr_ert_mean_z > 3 |
                   bisbas_ss_basm_rr_z < -3 |  bisbas_ss_basm_rr_z > 3 |
                   rt_diff_large_neutral_z < -3 |  rt_diff_large_neutral_z > 3 |
                   rt_diff_large_small_z < -3 |  rt_diff_large_small_z > 3 |
                   accumbens_rvsn_ant_z< -3 |  accumbens_rvsn_ant_z > 3 |
                   caudate_rvsn_ant_z < -3 |  caudate_rvsn_ant_z > 3 |
                   putamen_rvsn_ant_z < -3 |  putamen_rvsn_ant_z > 3 |
                   mOFC_rvsn_ant_z < -3 |  mOFC_rvsn_ant_z > 3 |
                   lOFC_rvsn_ant_z < -3 |  lOFC_rvsn_ant_z > 3 |
                   accumbens_posvsneg_feedback_z < -3 |  accumbens_posvsneg_feedback_z > 3 |
                   caudate_posvsneg_feedback_z < -3 |  caudate_posvsneg_feedback_z > 3 |
                   putamen_posvsneg_feedback_z < -3 |  putamen_posvsneg_feedback_z > 3 |
                   mOFC_posvsneg_feedback_z < -3 |  mOFC_posvsneg_feedback_z > 3 |
                   lOFC_posvsneg_feedback_z < -3 | lOFC_posvsneg_feedback_z> 3) ~ "outlier",
    TRUE ~ "not outlier"))

both_samples$is_outlier_any <- as.factor(both_samples$is_outlier_any)

# both_samples %>% count(is_outlier_any)
# not outlier: 10,324.
# outlier: 898.

#table <- compareGroups(is_outlier_any ~ ., data = both_samples,na.omit(TRUE))
#pvals <- getResults(table, "p.overall")
#p.adjust(pvals, method = "BH")
#export_table <- createTable(table)
#export_table

sample1_new <- subset(both_samples, sample == "sample1")
sample2_new <- subset(both_samples, sample == "sample2")

demographic_differences_sample1 <- compareGroups(is_outlier_any ~ sex +
                                           interview_age +
                                           bmi +
                                           race.ethnicity.5level +
                                           household.income+
                                           high.educ +
                                           demo_race_hispanic, 
                     data = sample1_new)
#View(demographic_differences_sample1)

pvals <- getResults(demographic_differences_sample1, "p.overall")
p.adjust(pvals, method = "BH")
export_table_sample1 <- createTable(demographic_differences_sample1)
export_table_sample1


demographic_differences_sample2 <- compareGroups(is_outlier_any ~ sex +
                                                   interview_age +
                                                   bmi +
                                                   race.ethnicity.5level +
                                                   household.income+
                                                   high.educ +
                                                   demo_race_hispanic, 
                                                 data = sample2_new)
#View(demographic_differences_sample2)

pvals <- getResults(demographic_differences_sample2, "p.overall")
p.adjust(pvals, method = "BH")
export_table_sample2 <- createTable(demographic_differences_sample2)
export_table_sample2


sample1_new <- sample1_new %>%
  mutate(is_outlier_accumbens_ant = case_when(
    (accumbens_rvsn_ant_z < -3 |  accumbens_rvsn_ant_z > 3) ~ "outlier",
    TRUE ~ "not outlier"))

demographic_differences_sample1_accumbens_ant <- compareGroups(is_outlier_accumbens_ant ~ sex +
                                                   interview_age +
                                                   bmi +
                                                   race.ethnicity.5level +
                                                   household.income+
                                                   high.educ +
                                                   demo_race_hispanic, 
                                                 data = sample1_new)

pvals <- getResults(demographic_differences_sample1_accumbens_ant, "p.overall")
p.adjust(pvals, method = "BH")
export_table_sample1_accumbens_ant<- createTable(demographic_differences_sample1_accumbens_ant)
export_table_sample1_accumbens_ant

# Sample 2 BMI differences.

descriptive_statistics_boxplot <- ggplot(sample2_new, aes(x = is_outlier_any, 
                                                          y = bmi,
                                                          #fill = is_outlier_any, 
                                                          color = is_outlier_any)) + 
  #geom_boxplot(outlier.shape = NA) +
  geom_violin() +
  #facet_grid(cols = vars(hemisphere), rows = vars(comparison)) +
  xlab("Outlier group") +
  ylab("BMI") + 
  labs(title = "Sample 2: BMI Distributions by Outlier Group") +
  theme(text = element_text(family = "Avenir"), 
        panel.spacing = unit(2, "lines"), 
        #title = element_text(size=22, vjust=2, face="bold"),
        #plot.subtitle = element_text(color="gray40", size=18, face="bold.italic"),
        axis.title.x= element_text(size=10, vjust=-0.3),
        axis.title.y= element_text(size=10, vjust=1.5),
        axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1, size=10, colour="black"),
        axis.text.y= element_text(size=10, colour="black"),
        strip.text = element_text(size=10, face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

descriptive_statistics_boxplot


