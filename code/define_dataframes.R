# Call this function after you have loaded "nda20_exploratory.csv" into data frame named "fulldata".
# Instead of writing this long code for every single script, we are going to call one script to do it.
# This way, we can make sure we are all loading in the exact same data and working with the exact same dataframes because we are using identical code (from this one script).
# If we need to edit anything about our dataframes, we can do so in that single script.

# Covariates.
#      Random effects.
#           1. Family. (rel_family_id) # Make sure this is nested correctly and looks about right.
#           2. Site. (site_id_l)
#       
#  Random effects are nested: (1|abcd_site/rel_family_id).

#      Fixed effects.
#           1. Race parent. (race.ethnicity.5level).
#           2. Education parent. (high.educ).
#           3. Income parent. (household.income)
#           4. Marital status parent. (married.or.livingtogether)
#           5. Age. (interview_age).
#           6. Hispanic. (demo_race_hispanic).


nrow(fulldata) # 5934.
# Because this is such a big data file, let's only keep the columns that we need for this analysis.
data <- fulldata[,c("src_subject_id",
                    "interview_age",
                    "eventname",
                    "sex",
                    "site_id_l",
                    "mri_info_deviceserialnumber",
                    "rel_family_id",
                    "race.ethnicity.5level",
                    "race.eth.7level",
                    "demo_race_hispanic", # Do you consider the child Hispanic/Latino/Latina?
                    "high.educ",
                    "household.income",
                    "married.or.livingtogether",
                    "cbcl_scr_syn_internal_t",
                    "cbcl_scr_syn_anxdep_t",
                    "cbcl_scr_syn_withdep_t",
                    "cbcl_scr_dsm5_depress_t",
                    "cbcl_scr_dsm5_anxdisord_t",
                    "cbcl_scr_syn_internal_r",
                    "cbcl_scr_syn_anxdep_r",
                    "cbcl_scr_syn_withdep_r",
                    "cbcl_scr_dsm5_depress_r",
                    "cbcl_scr_dsm5_anxdisord_r",
                    "PDS_score_f",
                    "PDS_score_m",
                    "PDS_score",
                    "PDS_sum_f",
                    "PDS_sum_m",
                    "PDS_sum",
                    "pds_p_ss_category",
                    "fam_history_q6d_depression",
                    "tfmri_ma_acdn_b_scs_aarh", # Accumbens reward vs. neutral anticipation.
                    "tfmri_ma_acdn_b_scs_aalh",
                    "tfmri_ma_acdn_b_scs_cdrh", # Caudate reward vs. neutral anticipation.
                    "tfmri_ma_acdn_b_scs_cdlh",
                    "tfmri_ma_acdn_b_scs_ptrh", # Putamen reward vs. neutral anticipation.
                    "tfmri_ma_acdn_b_scs_ptlh",
                    "tfmri_ma_arvn_b_cds_mobofrrh", # Medial OFC reward vs. neutral anticipation.
                    "tfmri_ma_arvn_b_cds_mobofrlh",
                    "tfmri_ma_arvn_b_cds_lobofrrh", # Lateral OFC reward vs. neutral anticipation.
                    "tfmri_ma_arvn_b_cds_lobofrlh",
                    "tfmri_ma_rpvnfb_b_scs_aarh", # Accumbens reward positive versus negative feedback.
                    "tfmri_ma_rpvnfb_b_scs_aalh",
                    "tfmri_ma_rpvnfb_b_scs_cdrh", # Caudate reward positive versus negative feedback.
                    "tfmri_ma_rpvnfb_b_scs_cdlh",
                    "tfmri_ma_rpvnfb_b_scs_ptrh", # Putamen reward positive versus negative feedback.
                    "tfmri_ma_rpvnfb_b_scs_ptlh",
                    "tfmri_ma_rpvnfb_b_cds_mobofrrh", # Medial OFC reward positive versus negative feedback.
                    "tfmri_ma_rpvnfb_b_cds_mobofrlh",
                    "tfmri_ma_rpvnfb_b_cds_lobofrrh", # Lateral OFC reward positive versus negative feedback.
                    "tfmri_ma_rpvnfb_b_cds_lobofrlh",
                    "accumbens_rvsn_ant_z", # Reward vs. neutral during anticipation stage (z scores).
                    "caudate_rvsn_ant_z",
                    "putamen_rvsn_ant_z",
                    "mOFC_rvsn_ant_z",
                    "lOFC_rvsn_ant_z",
                    "striatum_rvsn_ant_z", # reward vs. neutral anticipation.
                    "accumbens_posvsneg_feedback_z", # All positive vs. negative feedback.
                    "caudate_posvsneg_feedback_z",
                    "putamen_posvsneg_feedback_z",
                    "mOFC_posvsneg_feedback_z",
                    "lOFC_posvsneg_feedback_z",
                    "striatum_posvsneg_feedback_z",
                    "bisbas_ss_basm_rr",
                    "hormone_scr_ert_mean",
                    "hormone_scr_dhea_mean",
                    "tfmri_mid_all_beh_large.reward.pos.feedback_mean.rt", #Average MID RT Large Reward Positive
                    "tfmri_mid_all_beh_small.reward.pos.feedback_mean.rt", #Average MID RT Small Reward Positive
                    "tfmri_mid_all_beh_neutral.pos.feedback_mean.rt",#Average MID RT Neutral Positive
                    "tfmri_mid_beh_performflag", # Exclude or include based on MID behavioral data (1 = good; 0 = exclude).
                    #  0    1 NA's 
                    # 288 4250 1397
                    "imgincl_mid_include" # Exclude or include based on neuroimaging data (1 = good; 0 = exclude).
)]


data[c("src_subject_id","rel_family_id","eventname",
       "sex","demo_race_hispanic","site_id_l",
       "mri_info_deviceserialnumber","race.eth.7level",
       "high.educ","household.income","married.or.livingtogether",
       "pds_p_ss_category","tfmri_mid_beh_performflag","imgincl_mid_include")] <- lapply(data[c("src_subject_id","rel_family_id","eventname",
                                                                                                "sex","demo_race_hispanic","site_id_l",
                                                                                                "mri_info_deviceserialnumber","race.eth.7level",
                                                                                                "high.educ","household.income","married.or.livingtogether",
                                                                                                "pds_p_ss_category","tfmri_mid_beh_performflag","imgincl_mid_include")], as.factor) 


#data$src_subject_id <- as.factor(data$src_subject_id)
#data$rel_family_id <- as.factor(data$rel_family_id)
#data$eventname <- as.factor(data$eventname)
#data$sex <- as.factor(data$sex)
#data$demo_race_hispanic <- as.factor(data$demo_race_hispanic)
#data$site_id_l <- as.factor(data$site_id_l)
#data$mri_info_deviceserialnumber <- as.factor(data$mri_info_deviceserialnumber)
#data$race.eth.7level <- as.factor(data$race.eth.7level)
#data$high.educ <- as.factor(data$high.educ)
#data$household.income <- as.factor(data$household.income)
#data$married.or.livingtogether <- as.factor(data$married.or.livingtogether)
#data$pds_p_ss_category <-as.factor(data$pds_p_ss_category)
#data$tfmri_mid_beh_performflag <- as.factor(data$tfmri_mid_beh_performflag)
#data$imgincl_mid_include <- as.factor(data$imgincl_mid_include)

data$race.ethnicity.5level = data$race.eth.7level
data$race.ethnicity.5level[(data$race.eth.7level == "AIAN" | data$race.eth.7level == "NHPI")] = "Other"
data$race.ethnicity.5level = droplevels(data$race.ethnicity.5level)

data$PDS_score_z<- scale(data$PDS_score)
data$cbcl_scr_syn_internal_r_z <- scale(data$cbcl_scr_syn_internal_r)
data$hormone_scr_ert_mean_z <- scale(data$hormone_scr_ert_mean)

nrow(data) # 5934.

# Use data with only correct PDS scores.
PDS_correct <- subset(data, PDS_score < 5) #Be mindful that PDS category goes from 1 to 5, while PDS_score goes from 1 to 4 (continuous).
nrow(PDS_correct) # Exploratory: 4224. Confirmatory: 4244.

PDS_correct <- subset(PDS_correct, PDS_sum<30) # This shouldn't change anything but just in case there is somehow a participant with a PDS score less than 5 but a sum that is incorrect.
nrow(PDS_correct) # Exploratory: 4224. Confirmatory: 4244.
PDS_correct <- PDS_correct %>% filter(sex!="") #remove 6 participants with no gender.
nrow(PDS_correct) # Exploratory: 4224. Confirmatory: 4244. Were these participants already removed from the dataframe?

# There are two people with a PDS category score of 5, which will bias the category estimates a lot, so we are removing them.
PDS_correct$pds_p_ss_category <- as.factor(PDS_correct$pds_p_ss_category)
PDS_correct <- subset(PDS_correct, pds_p_ss_category != "5")
nrow(PDS_correct) # 4222.
mean_PDS_score <- mean(PDS_correct$PDS_score)
sd_PDS_score <- sd(PDS_correct$PDS_score)
PDS_correct$PDS_score_z <- (PDS_correct$PDS_score-mean_PDS_score)/sd_PDS_score 

# Exclude people ABCD says to exclude for MID task.
MID_task_correct <- subset(PDS_correct, tfmri_mid_beh_performflag ==1)
nrow(MID_task_correct) # 3945 (exploratory).
#summary(PDS_correct$tfmri_mid_beh_performflag)
#  0    1     NA's 
#  262 3945   15 

# MID Reaction Time Variable Creation  (including here, after MID_task_correct is created, so that when we scale, we are using the data frame that only includes the correct MID task data).
## Reaction time difference between large reward trials and neutral trials (Positive value indicates greater sensitivity to large reward than neutral trials).
MID_task_correct$rt_diff_large_neutral <- MID_task_correct$tfmri_mid_all_beh_neutral.pos.feedback_mean.rt - MID_task_correct$tfmri_mid_all_beh_large.reward.pos.feedback_mean.rt

## Reaction time difference between large reward trials and neutral trials (Positive value indicates greater sensitivity to large reward than small trials)
MID_task_correct$rt_diff_large_small <- MID_task_correct$tfmri_mid_all_beh_small.reward.pos.feedback_mean.rt - MID_task_correct$tfmri_mid_all_beh_large.reward.pos.feedback_mean.rt

MID_task_correct$rt_diff_large_neutral_z <- scale(MID_task_correct$rt_diff_large_neutral)
MID_task_correct$rt_diff_large_small_z <- scale(MID_task_correct$rt_diff_large_small)


# Exclude based on ABCD's recommendation for MID imaging, including only those with both usable task and imaging data here.
#summary(PDS_correct$imgincl_mid_include)
#0    1      NA's 
#517 3701    4 
# nrow(MID_task_correct)
MID_imaging_correct <- subset(MID_task_correct, imgincl_mid_include ==1) 
nrow(MID_imaging_correct) # 3687 (exploratory). so 258 drop out after imaging parameters are taken into account.
# Are there people excluded by task who are still included by imaging? If so, we want to exclude them for imaging as well (only want people who did task correctly in imaging analyses).

# In confirmatory, goes from 4242 (PDS_correct) to 3959 (MID_correct).

# Separate by sex.
PDS_correct_females <- subset(PDS_correct, sex == "F")
PDS_correct_males <- subset(PDS_correct, sex == "M")
nrow(PDS_correct_females)  # 2058 (exploratory); 2059 (confirmatory).
nrow(PDS_correct_males) # 2164 (exploratory); 2165 (confirmatory).

MID_task_correct_females <- subset(MID_task_correct, sex == "F")
MID_task_correct_males <- subset(MID_task_correct, sex == "M")
nrow(MID_task_correct_females)  # 1949 (exploratory).
nrow(MID_task_correct_males) # 1996 (exploratory).

MID_imaging_correct_females <- subset(MID_imaging_correct, sex == "F")
MID_imaging_correct_males <- subset(MID_imaging_correct, sex == "M")
nrow(MID_imaging_correct_females)  # 1849 (exploratory).
nrow(MID_imaging_correct_males) # 1838 (exploratory).


# Create different subsets of the data based on removing outliers for specific variables of interest.
# For anything that is not related to the MID task, use PDS_correct.
# For anything that is related to the behavioral data from the MID task (and not the imaging data), use MID_task_correct.
# For anything that is related to the imaging data from the MID task, use MID_imaging_correct.

# No striatal anticipation outliers.
data_no_striatal_ant_outliers <- subset(MID_imaging_correct, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3)
nrow(data_no_striatal_ant_outliers) # 4169.

data_no_striatal_ant_outliers_females <- subset(MID_imaging_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3)
nrow(data_no_striatal_ant_outliers_females) # 2035.

data_no_striatal_ant_outliers_males <- subset(MID_imaging_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3)
nrow(data_no_striatal_ant_outliers_males) # 2134.

# No striatal FEEDBACK outliers.
data_no_striatal_feed_outliers <- subset(MID_imaging_correct, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3)
nrow(data_no_striatal_feed_outliers) # 4181.

data_no_striatal_feed_outliers_females <- subset(MID_imaging_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3)
nrow(data_no_striatal_feed_outliers_females) # 2041.

data_no_striatal_feed_outliers_males <- subset(MID_imaging_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3)
nrow(data_no_striatal_feed_outliers_males) # 2140.

data_no_striatal_feed_outliers_females_pubertal <- subset(MID_imaging_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3  & pds_p_ss_category != 1)
nrow(data_no_striatal_feed_outliers_females_pubertal) # 1412.
data_no_striatal_feed_outliers_males_pubertal <- subset(MID_imaging_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3  &  pds_p_ss_category != 1)
nrow(data_no_striatal_feed_outliers_males_pubertal) # 605.

data_no_striatal_feed_outliers_females_prepubertal <- subset(MID_imaging_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3   & pds_p_ss_category == 1)
nrow(data_no_striatal_feed_outliers_females_prepubertal) # 629.
data_no_striatal_feed_outliers_males_prepubertal <- subset(MID_imaging_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3   &  pds_p_ss_category == 1)
nrow(data_no_striatal_feed_outliers_males_prepubertal) # 1535.

# No lateral OFC (lOFC) anticipation outliers.
data_no_lOFC_ant_outliers <- subset(MID_imaging_correct, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 33)
nrow(data_no_lOFC_ant_outliers) # 4165.

data_no_lOFC_ant_outliers_females <- subset(MID_imaging_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3)
nrow(data_no_lOFC_ant_outliers_females) # 2025.

data_no_lOFC_ant_outliers_males <- subset(MID_imaging_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3)
nrow(data_no_lOFC_ant_outliers_males) # 2105.

data_no_lOFC_ant_outliers_females_pubertal <- subset(MID_imaging_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & pds_p_ss_category != 1)
nrow(data_no_lOFC_ant_outliers_females_pubertal) # 1400.
data_no_lOFC_ant_outliers_males_pubertal <- subset(MID_imaging_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 &  pds_p_ss_category != 1)
nrow(data_no_lOFC_ant_outliers_males_pubertal) # 585.

data_no_lOFC_ant_outliers_females_prepubertal <- subset(MID_imaging_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & pds_p_ss_category == 1)
nrow(data_no_lOFC_ant_outliers_females_prepubertal) # 625.
data_no_lOFC_ant_outliers_males_prepubertal <- subset(MID_imaging_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 &  pds_p_ss_category == 1)
nrow(data_no_lOFC_ant_outliers_males_prepubertal) # 1520.

# No medial OFC (mOFC) anticipation outliers.
data_no_mOFC_ant_outliers <- subset(MID_imaging_correct, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 33)
nrow(data_no_mOFC_ant_outliers) # 4170.

data_no_mOFC_ant_outliers_females <- subset(MID_imaging_correct_females, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3)
nrow(data_no_mOFC_ant_outliers_females) # 2028.

data_no_mOFC_ant_outliers_males <- subset(MID_imaging_correct_males, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3)
nrow(data_no_mOFC_ant_outliers_males) # 2116.

data_no_mOFC_ant_outliers_females_pubertal <- subset(MID_imaging_correct_females, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & pds_p_ss_category != 1)
nrow(data_no_mOFC_ant_outliers_females_pubertal) # 1405.
data_no_mOFC_ant_outliers_males_pubertal <- subset(MID_imaging_correct_males, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 &  pds_p_ss_category != 1)
nrow(data_no_mOFC_ant_outliers_males_pubertal) # 593.

data_no_mOFC_ant_outliers_females_prepubertal <- subset(MID_imaging_correct_females, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & pds_p_ss_category == 1)
nrow(data_no_mOFC_ant_outliers_females_prepubertal) # 623.
data_no_mOFC_ant_outliers_males_prepubertal <- subset(MID_imaging_correct_males, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 &  pds_p_ss_category == 1)
nrow(data_no_mOFC_ant_outliers_males_prepubertal) # 1523.

# No lateral OFC (lOFC) FEEDBACK outliers.
data_no_lOFC_feed_outliers <- subset(MID_imaging_correct, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3)
nrow(data_no_lOFC_feed_outliers) # 4156.

data_no_lOFC_feed_outliers_females <- subset(MID_imaging_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3)
nrow(data_no_lOFC_feed_outliers_females) # 2302.

data_no_lOFC_feed_outliers_males <- subset(MID_imaging_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3)
nrow(data_no_lOFC_feed_outliers_males) # 2124.

data_no_lOFC_feed_outliers_females_pubertal <- subset(MID_imaging_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & pds_p_ss_category != 1)
nrow(data_no_lOFC_feed_outliers_females_pubertal) # 1406.
data_no_lOFC_feed_outliers_males_pubertal <- subset(MID_imaging_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 &  pds_p_ss_category != 1)
nrow(data_no_lOFC_feed_outliers_males_pubertal) # 596.

data_no_lOFC_feed_outliers_females_prepubertal <- subset(MID_imaging_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & pds_p_ss_category == 1)
nrow(data_no_lOFC_feed_outliers_females_prepubertal) # 626.
data_no_lOFC_feed_outliers_males_prepubertal <- subset(MID_imaging_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 &  pds_p_ss_category == 1)
nrow(data_no_lOFC_feed_outliers_males_prepubertal) # 1528.

# No medial OFC (mOFC) FEEDBACK outliers.
data_no_mOFC_feed_outliers <- subset(MID_imaging_correct, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3)
nrow(data_no_mOFC_feed_outliers) #4157.

data_no_mOFC_feed_outliers_females <- subset(MID_imaging_correct_females, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3)
nrow(data_no_mOFC_feed_outliers_females) # 2034.

data_no_mOFC_feed_outliers_males <- subset(MID_imaging_correct_males, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3)
nrow(data_no_mOFC_feed_outliers_males) # 2123.

data_no_mOFC_feed_outliers_females_pubertal <- subset(MID_imaging_correct_females, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & pds_p_ss_category != 1)
nrow(data_no_mOFC_feed_outliers_females_pubertal) # 1408.
data_no_mOFC_feed_outliers_males_pubertal <- subset(MID_imaging_correct_males, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 &  pds_p_ss_category != 1)
nrow(data_no_mOFC_feed_outliers_males_pubertal) # 596.

data_no_mOFC_feed_outliers_females_prepubertal <- subset(MID_imaging_correct_females, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & pds_p_ss_category == 1)
nrow(data_no_mOFC_feed_outliers_females_prepubertal) # 626.
data_no_mOFC_feed_outliers_males_prepubertal <- subset(MID_imaging_correct_males, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 &  pds_p_ss_category == 1)
nrow(data_no_mOFC_feed_outliers_males_prepubertal) # 1527.

# No testosterone outliers.
data_no_test_outliers <- subset(PDS_correct, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_test_outliers_females <- subset(PDS_correct_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_test_outliers_males <- subset(PDS_correct_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# Get z scores for bis bas rr scores.
mean_bisbas <- mean(PDS_correct$bisbas_ss_basm_rr, na.rm=TRUE)
sd_bisbas <- sd(PDS_correct$bisbas_ss_basm_rr, na.rm=TRUE)
PDS_correct$bisbas_ss_basm_rr_z <- (PDS_correct$bisbas_ss_basm_rr-mean_bisbas)/sd_bisbas

data_no_bisbas_outliers <- subset(PDS_correct, bisbas_ss_basm_rr_z > -3 & bisbas_ss_basm_rr_z < 3)
nrow(data_no_bisbas_outliers) # 4204.
data_no_bisbas_outliers_females <- subset(data_no_bisbas_outliers, sex == "F")
nrow(data_no_bisbas_outliers_females) # 2049.
data_no_bisbas_outliers_males <- subset(data_no_bisbas_outliers, sex == "M")
nrow(data_no_bisbas_outliers_males) # 2155.

# No striatal anticipation or testosterone outliers.
data_no_striatal_ant_test_outliers <- subset(data_no_striatal_ant_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_striatal_ant_test_outliers_females <- subset(data_no_striatal_ant_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_striatal_ant_test_outliers_males <- subset(data_no_striatal_ant_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No striatal feedback or testosterone outliers.
data_no_striatal_feed_test_outliers <- subset(data_no_striatal_feed_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_striatal_feed_test_outliers_females <- subset(data_no_striatal_feed_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_striatal_feed_test_outliers_males <- subset(data_no_striatal_feed_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No lOFC anticipation or testosterone outliers.
data_no_lOFC_ant_test_outliers <- subset(data_no_lOFC_ant_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_lOFC_ant_test_outliers_females <- subset(data_no_lOFC_ant_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_lOFC_ant_test_outliers_males <- subset(data_no_lOFC_ant_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No lOFC feedback or testosterone outliers.
data_no_lOFC_feed_test_outliers <- subset(data_no_lOFC_feed_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_lOFC_feed_test_outliers_females <- subset(data_no_lOFC_feed_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_lOFC_feed_test_outliers_males <- subset(data_no_lOFC_feed_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No mOFC anticipation or testosterone outliers.
data_no_mOFC_ant_test_outliers <- subset(data_no_mOFC_ant_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_mOFC_ant_test_outliers_females <- subset(data_no_mOFC_ant_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_mOFC_ant_test_outliers_males <- subset(data_no_mOFC_ant_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No mOFC feedback or testosterone outliers.
data_no_mOFC_feed_test_outliers <- subset(data_no_mOFC_feed_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_mOFC_feed_test_outliers_females <- subset(data_no_mOFC_feed_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_mOFC_feed_test_outliers_males <- subset(data_no_mOFC_feed_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No BIS/BAS or testosterone outliers.
data_no_bisbas_test_outliers <- subset(data_no_bisbas_outliers, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_bisbas_test_outliers_females <- subset(data_no_bisbas_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_bisbas_test_outliers_males <- subset(data_no_bisbas_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

# No MID Reaction Time outliers.
data_no_RT_MID_outliers_females <- subset(MID_task_correct_females, rt_diff_large_neutral_z > -3 & rt_diff_large_neutral_z < 3 & rt_diff_large_small_z > -3 & rt_diff_large_small_z < 3)
data_no_RT_MID_outliers_males <- subset(MID_task_correct_males, rt_diff_large_neutral_z > -3 & rt_diff_large_neutral_z < 3 & rt_diff_large_small_z > -3 & rt_diff_large_small_z < 3)

# No MID Reaction Time or testosterone outliers.
data_no_RT_test_outliers_females <- subset(data_no_RT_MID_outliers_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)
data_no_RT_test_outliers_males <- subset(data_no_RT_MID_outliers_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3)

#### Separate reward regions for anticipation reward vs neutral. #### 
# No accumbens outliers.
data_no_accumbens_ant_outliers_females <- subset(MID_imaging_correct_females, accumbens_rvsn_ant_z > -3 & accumbens_rvsn_ant_z < 3)
nrow(data_no_accumbens_ant_outliers_females) # 2030
data_no_accumbens_ant_outliers_males <- subset(MID_imaging_correct_males, accumbens_rvsn_ant_z > -3 & accumbens_rvsn_ant_z < 3)
nrow(data_no_accumbens_ant_outliers_males) # 2123

# No caudate outliers.
data_no_caudate_ant_outliers_females <- subset(MID_imaging_correct_females, caudate_rvsn_ant_z > -3 & caudate_rvsn_ant_z < 3)
nrow(data_no_caudate_ant_outliers_females) # 2039
data_no_caudate_ant_outliers_males <- subset(MID_imaging_correct_males, caudate_rvsn_ant_z > -3 & caudate_rvsn_ant_z < 3)
nrow(data_no_caudate_ant_outliers_males) # 2137

# No putamen outliers.
data_no_putamen_ant_outliers_females <- subset(MID_imaging_correct_females, putamen_rvsn_ant_z > -3 & putamen_rvsn_ant_z < 3)
nrow(data_no_putamen_ant_outliers_females) # 2037
data_no_putamen_ant_outliers_males <- subset(MID_imaging_correct_males, putamen_rvsn_ant_z > -3 & putamen_rvsn_ant_z < 3)
nrow(data_no_putamen_ant_outliers_males) # 2139

#### Separate reward regions for feedback positive vs negative.####
# No accumbens outliers.
data_no_accumbens_feed_outliers <- subset(MID_imaging_correct, accumbens_posvsneg_feedback_z > -3 & accumbens_posvsneg_feedback_z < 3) 
nrow(data_no_accumbens_feed_outliers) # 4163
data_no_accumbens_feed_outliers_females <- subset(MID_imaging_correct_females, accumbens_posvsneg_feedback_z > -3 & accumbens_posvsneg_feedback_z < 3)
nrow(data_no_accumbens_feed_outliers_females) # 2033.
data_no_accumbens_feed_outliers_males <- subset(MID_imaging_correct_males, accumbens_posvsneg_feedback_z > -3 & accumbens_posvsneg_feedback_z < 3)
nrow(data_no_accumbens_feed_outliers_males) # 2130

# No caudate outliers.
data_no_caudate_feed_outliers <- subset(MID_imaging_correct, caudate_posvsneg_feedback_z > -3 & caudate_posvsneg_feedback_z < 3) 
nrow(data_no_caudate_feed_outliers) # 4188
data_no_caudate_feed_outliers_females <- subset(MID_imaging_correct_females, caudate_posvsneg_feedback_z > -3 & caudate_posvsneg_feedback_z < 3)
nrow(data_no_caudate_feed_outliers_females) # 2043
data_no_caudate_feed_outliers_males <- subset(MID_imaging_correct_males, caudate_posvsneg_feedback_z > -3 & caudate_posvsneg_feedback_z < 3)
nrow(data_no_caudate_feed_outliers_males) # 2145

# No putamen outliers.
data_no_putamen_feed_outliers <- subset(MID_imaging_correct, putamen_posvsneg_feedback_z > -3 & putamen_posvsneg_feedback_z < 3) 
nrow(data_no_putamen_feed_outliers) # 4190
data_no_putamen_feed_outliers_females <- subset(MID_imaging_correct_females, putamen_posvsneg_feedback_z > -3 & putamen_posvsneg_feedback_z < 3)
nrow(data_no_putamen_feed_outliers_females) # 2044
data_no_putamen_feed_outliers_males <- subset(MID_imaging_correct_males, putamen_posvsneg_feedback_z > -3 & putamen_posvsneg_feedback_z < 3)
nrow(data_no_putamen_feed_outliers_males) # 2146


#### Note: We are including all CBCL values (as long as they are in a reasonable range, i.e., possible given the scale). Not excluding CBCL outliers. #### 
# No CBCL outliers.
#data_no_CBCL_outliers <- subset(PDS_correct, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
#nrow(data_no_CBCL_outliers) # 4152.
#cbcl_outliers <- subset(PDS_correct, cbcl_scr_syn_internal_r_z < -3 | cbcl_scr_syn_internal_r_z > 3)

#data_no_CBCL_outliers_females<- subset(PDS_correct_females, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
#nrow(data_no_CBCL_outliers_females) # 2026.

#data_no_CBCL_outliers_males<- subset(PDS_correct_males, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
#nrow(data_no_CBCL_outliers_males) # 2126.
# 
# 
# # No CBCL or striatal anticipation outliers.
# data_no_CBCL_striatal_ant_outliers <- subset(PDS_correct, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_striatal_ant_outliers) # 4097.
# 
# data_no_CBCL_striatal_ant_outliers_females <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_striatal_ant_outliers_females) # 2002.
# 
# data_no_CBCL_striatal_ant_outliers_males <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_striatal_ant_outliers_males) # 2095.
# 
# data_no_CBCL_striatal_ant_outliers_females_pubertal <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
# nrow(data_no_CBCL_striatal_ant_outliers_females_pubertal) #1380
# data_no_CBCL_striatal_ant_outliers_males_pubertal <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
# nrow(data_no_CBCL_striatal_ant_outliers_males_pubertal) #583
# 
# data_no_CBCL_striatal_ant_outliers_females_prepubertal <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
# nrow(data_no_CBCL_striatal_ant_outliers_females_prepubertal) #1380
# data_no_CBCL_striatal_ant_outliers_males_prepubertal <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
# nrow(data_no_CBCL_striatal_ant_outliers_males_prepubertal) #583
# 
# 
# # No CBCL or striatal FEEDBACK outliers.
# data_no_CBCL_striatal_feed_outliers <- subset(PDS_correct, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_striatal_feed_outliers) # 4109
# 
# data_no_CBCL_striatal_feed_outliers_females <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_striatal_feed_outliers_females) # 2008.
# 
# data_no_CBCL_striatal_feed_outliers_males <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_striatal_feed_outliers_males) # 2101.
# 
# data_no_CBCL_striatal_feed_outliers_females_pubertal <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
# nrow(data_no_CBCL_striatal_feed_outliers_females_pubertal) #1385
# data_no_CBCL_striatal_feed_outliers_males_pubertal <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
# nrow(data_no_CBCL_striatal_feed_outliers_males_pubertal) #586
# 
# data_no_CBCL_striatal_feed_outliers_females_prepubertal <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
# nrow(data_no_CBCL_striatal_feed_outliers_females_prepubertal) #623
# data_no_CBCL_striatal_feed_outliers_males_prepubertal <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
# nrow(data_no_CBCL_striatal_feed_outliers_males_prepubertal) #1515
# 
# 
# # No CBCL or or lateral OFC (lOFC) anticipation outliers.
# data_no_CBCL_lOFC_ant_outliers <- subset(PDS_correct, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_lOFC_ant_outliers) # 4061.
# 
# data_no_CBCL_lOFC_ant_outliers_females <- subset(PDS_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_lOFC_ant_outliers_females) # 1992.
# 
# data_no_CBCL_lOFC_ant_outliers_males <- subset(PDS_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_lOFC_ant_outliers_males) # 2069.
# 
# data_no_CBCL_lOFC_ant_outliers_females_pubertal <- subset(PDS_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
# nrow(data_no_CBCL_lOFC_ant_outliers_females_pubertal) #1373
# data_no_CBCL_lOFC_ant_outliers_males_pubertal <- subset(PDS_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
# nrow(data_no_CBCL_lOFC_ant_outliers_males_pubertal) #568
# 
# data_no_CBCL_lOFC_ant_outliers_females_prepubertal <- subset(PDS_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
# nrow(data_no_CBCL_lOFC_ant_outliers_females_prepubertal) #619
# data_no_CBCL_lOFC_ant_outliers_males_prepubertal <- subset(PDS_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
# nrow(data_no_CBCL_lOFC_ant_outliers_males_prepubertal) #1501
# 
# # No CBCL or or medial OFC (mOFC) anticipation outliers.
# data_no_CBCL_mOFC_ant_outliers <- subset(PDS_correct, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_mOFC_ant_outliers) # 4074.
# 
# data_no_CBCL_mOFC_ant_outliers_females <- subset(PDS_correct_females, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_mOFC_ant_outliers_females) # 1995.
# 
# data_no_CBCL_mOFC_ant_outliers_males <- subset(PDS_correct_males, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_mOFC_ant_outliers_males) # 2079.
# 
# data_no_CBCL_mOFC_ant_outliers_females_pubertal <- subset(PDS_correct_females, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
# nrow(data_no_CBCL_mOFC_ant_outliers_females_pubertal) # 1378.
# data_no_CBCL_mOFC_ant_outliers_males_pubertal <- subset(PDS_correct_males, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
# nrow(data_no_CBCL_mOFC_ant_outliers_males_pubertal) # 575.
# 
# data_no_CBCL_mOFC_ant_outliers_females_prepubertal <- subset(PDS_correct_females, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
# nrow(data_no_CBCL_mOFC_ant_outliers_females_prepubertal) # 617.
# data_no_CBCL_mOFC_ant_outliers_males_prepubertal <- subset(PDS_correct_males, mOFC_rvsn_ant_z > -3 & mOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
# nrow(data_no_CBCL_mOFC_ant_outliers_males_prepubertal) # 1504.

# No CBCL or lateral OFC (lOFC) FEEDBACK outliers.
# data_no_CBCL_lOFC_feed_outliers <- subset(PDS_correct, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_lOFC_feed_outliers) #4086
# 
# data_no_CBCL_lOFC_feed_outliers_females <- subset(PDS_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_lOFC_feed_outliers_females) # 1999
# 
# data_no_CBCL_lOFC_feed_outliers_males <- subset(PDS_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_lOFC_feed_outliers_males) # 2087.
# 
# data_no_CBCL_lOFC_feed_outliers_females_pubertal <- subset(PDS_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
# nrow(data_no_CBCL_lOFC_feed_outliers_females_pubertal) #1379
# data_no_CBCL_lOFC_feed_outliers_males_pubertal <- subset(PDS_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
# nrow(data_no_CBCL_lOFC_feed_outliers_males_pubertal) #579
# 
# data_no_CBCL_lOFC_feed_outliers_females_prepubertal <- subset(PDS_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
# nrow(data_no_CBCL_lOFC_feed_outliers_females_prepubertal) #620
# data_no_CBCL_lOFC_feed_outliers_males_prepubertal <- subset(PDS_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
# nrow(data_no_CBCL_lOFC_feed_outliers_males_prepubertal) #1508
# 
# # No CBCL or medial OFC (mOFC) FEEDBACK outliers.
# data_no_CBCL_mOFC_feed_outliers <- subset(PDS_correct, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_mOFC_feed_outliers) # 4087.
# 
# data_no_CBCL_mOFC_feed_outliers_females <- subset(PDS_correct_females, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_mOFC_feed_outliers_females) # 2001.
# 
# data_no_CBCL_mOFC_feed_outliers_males <- subset(PDS_correct_males, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# nrow(data_no_CBCL_mOFC_feed_outliers_males) # 2086.
# 
# data_no_CBCL_mOFC_feed_outliers_females_pubertal <- subset(PDS_correct_females, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
# nrow(data_no_CBCL_mOFC_feed_outliers_females_pubertal) # 1381.
# data_no_CBCL_mOFC_feed_outliers_males_pubertal <- subset(PDS_correct_males, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
# nrow(data_no_CBCL_mOFC_feed_outliers_males_pubertal) # 579.
# 
# data_no_CBCL_mOFC_feed_outliers_females_prepubertal <- subset(PDS_correct_females, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
# nrow(data_no_CBCL_mOFC_feed_outliers_females_prepubertal) # 620.
# data_no_CBCL_mOFC_feed_outliers_males_prepubertal <- subset(PDS_correct_males, mOFC_posvsneg_feedback_z > -3 & mOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
# nrow(data_no_CBCL_mOFC_feed_outliers_males_prepubertal) # 1507.
# 
# # No CBCL or testosterone outliers.
# data_no_CBCL_test_outliers <- subset(PDS_correct, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# data_no_CBCL_test_outliers_females <- subset(PDS_correct_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# data_no_CBCL_test_outliers_males <- subset(PDS_correct_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# 
# 
# # No CBCL or BIS/BAS outliers.
# data_no_CBCL_bisbas_outliers <- subset(data_no_bisbas_outliers, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# data_no_CBCL_bisbas_outliers_females <- subset(data_no_bisbas_outliers_females, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
# data_no_CBCL_bisbas_outliers_males <- subset(data_no_bisbas_outliers_males, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
#
# No MID Reaction Time or CBCL outliers.
#data_no_RT_MID_CBCL_outliers_females<- subset(data_no_RT_MID_outliers_females, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
#data_no_RT_MID_CBCL_outliers_males<- subset(data_no_RT_MID_outliers_males, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
