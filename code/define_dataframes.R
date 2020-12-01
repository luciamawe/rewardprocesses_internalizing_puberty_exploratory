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
                    "cbcl_scr_syn_internal_r",
                    "cbcl_scr_syn_anxdep_r",
                    "cbcl_scr_syn_withdep_r",
                    "cbcl_scr_dsm5_depress_r",
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
                    "hormone_scr_dhea_mean"
)]

data$src_subject_id <- as.factor(data$src_subject_id)
data$rel_family_id <- as.factor(data$rel_family_id)
data$eventname <- as.factor(data$eventname)
data$sex <- as.factor(data$sex)
data$demo_race_hispanic <- as.factor(data$demo_race_hispanic)
data$site_id_l <- as.factor(data$site_id_l)
data$mri_info_deviceserialnumber <- as.factor(data$mri_info_deviceserialnumber)
data$race.eth.7level <- as.factor(data$race.eth.7level)
data$high.educ <- as.factor(data$high.educ)
data$household.income <- as.factor(data$household.income)
data$married.or.livingtogether <- as.factor(data$married.or.livingtogether)
data$pds_p_ss_category <-as.factor(data$pds_p_ss_category)
data$race.ethnicity.5level = data$race.eth.7level
data$race.ethnicity.5level[(data$race.eth.7level == "AIAN" | data$race.eth.7level == "NHPI")] = "Other"
data$race.ethnicity.5level = droplevels(data$race.ethnicity.5level)

data$PDS_score_z<- scale(data$PDS_score)
data$cbcl_scr_syn_internal_r_z <- scale(data$cbcl_scr_syn_internal_r)
data$hormone_scr_ert_mean_z <- scale(data$hormone_scr_ert_mean)

nrow(data) # 5934.

# Use data with only correct PDS scores.
PDS_correct <- subset(data, PDS_score < 5) #Be mindful that PDS category goes from 1 to 5, while PDS_average goes from 1 to 4 (continuous).
nrow(PDS_correct) # 4224.

PDS_correct <- subset(PDS_correct, PDS_sum<30) # This shouldn't change anything but just in case there is somehow a participant with a PDS score less than 5 but a sum that is incorrect.
nrow(PDS_correct) # 4224.
PDS_correct <- PDS_correct %>% filter(sex!="") #remove 6 participants with no gender.
nrow(PDS_correct) # 4224. Were these participants already removed from the dataframe?

# There are two people with a PDS category score of 5, which will bias the category estimates a lot, so we are removing them.
PDS_correct$pds_p_ss_category <- as.factor(PDS_correct$pds_p_ss_category)
PDS_correct <- subset(PDS_correct, pds_p_ss_category != "5")
nrow(PDS_correct) # 4222.
mean_PDS_score <- mean(PDS_correct$PDS_score)
sd_PDS_score <- sd(PDS_correct$PDS_score)
PDS_correct$PDS_score_z <- (PDS_correct$PDS_score-mean_PDS_score)/sd_PDS_score 


# Separate by sex.
PDS_correct_females <- subset(PDS_correct, sex == "F")
PDS_correct_males <- subset(PDS_correct, sex == "M")
nrow(PDS_correct_females)  # 2059.
nrow(PDS_correct_males) # 2165.

# Create different subsets of the data based on removing outliers for specific variables of interest.
# No CBCL outliers.
data_no_CBCL_outliers <- subset(PDS_correct, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_outliers) # 4152.

data_no_CBCL_outliers_females<- subset(PDS_correct_females, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_outliers_females) # 2026.

data_no_CBCL_outliers_males<- subset(PDS_correct_males, cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_outliers_males) # 2126.

# No striatal anticipation outliers.
data_no_striatal_ant_outliers <- subset(PDS_correct, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3)
nrow(data_no_striatal_ant_outliers) # 4169.

data_no_striatal_ant_outliers_females <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3)
nrow(data_no_striatal_ant_outliers_females) # 2035.

data_no_striatal_ant_outliers_males <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3)
nrow(data_no_striatal_ant_outliers_males) # 2134.

# No CBCL or striatal anticipation outliers.
data_no_CBCL_striatal_ant_outliers <- subset(PDS_correct, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_striatal_ant_outliers) # 4097.

data_no_CBCL_striatal_ant_outliers_females <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_striatal_ant_outliers_females) # 2002.

data_no_CBCL_striatal_ant_outliers_males <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_striatal_ant_outliers_males) # 2095.

data_no_CBCL_striatal_ant_outliers_females_pubertal <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
length(data_no_CBCL_striatal_ant_outliers_females_pubertal$src_subject_id) #1380
data_no_CBCL_striatal_ant_outliers_males_pubertal <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
length(data_no_CBCL_striatal_ant_outliers_males_pubertal$src_subject_id) #583

data_no_CBCL_striatal_ant_outliers_females_prepubertal <- subset(PDS_correct_females, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
length(data_no_CBCL_striatal_ant_outliers_females_prepubertal$src_subject_id) #1380
data_no_CBCL_striatal_ant_outliers_males_prepubertal <- subset(PDS_correct_males, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
length(data_no_CBCL_striatal_ant_outliers_males_prepubertal$src_subject_id) #583

# No striatal FEEDBACK outliers.
data_no_striatal_feed_outliers <- subset(PDS_correct, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3)
nrow(data_no_striatal_feed_outliers) # 4181.

data_no_striatal_feed_outliers_females <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3)
nrow(data_no_striatal_feed_outliers_females) # 2041.

data_no_striatal_feed_outliers_males <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3)
nrow(data_no_striatal_feed_outliers_males) # 2140.

data_no_striatal_feed_outliers_females_pubertal <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3  & pds_p_ss_category != 1)
nrow(data_no_striatal_feed_outliers_females_pubertal) # 1412.
data_no_striatal_feed_outliers_males_pubertal <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3  &  pds_p_ss_category != 1)
nrow(data_no_striatal_feed_outliers_males_pubertal) # 605.

data_no_striatal_feed_outliers_females_prepubertal <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3   & pds_p_ss_category == 1)
nrow(data_no_striatal_feed_outliers_females_prepubertal) # 629.
data_no_striatal_feed_outliers_males_prepubertal <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3   &  pds_p_ss_category == 1)
nrow(data_no_striatal_feed_outliers_males_prepubertal) # 1535.

# No CBCL or striatal FEEDBACK outliers.
data_no_CBCL_striatal_feed_outliers <- subset(PDS_correct, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_striatal_feed_outliers) # 4109

data_no_CBCL_striatal_feed_outliers_females <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_striatal_feed_outliers_females) # 2008.

data_no_CBCL_striatal_feed_outliers_males <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_striatal_feed_outliers_males) # 2101.

data_no_CBCL_striatal_feed_outliers_females_pubertal <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
length(data_no_CBCL_striatal_feed_outliers_females_pubertal$src_subject_id) #1385
data_no_CBCL_striatal_feed_outliers_males_pubertal <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
length(data_no_CBCL_striatal_feed_outliers_males_pubertal$src_subject_id) #586

data_no_CBCL_striatal_feed_outliers_females_prepubertal <- subset(PDS_correct_females, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
length(data_no_CBCL_striatal_feed_outliers_females_prepubertal$src_subject_id) #623
data_no_CBCL_striatal_feed_outliers_males_prepubertal <- subset(PDS_correct_males, striatum_posvsneg_feedback_z > -3 & striatum_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
length(data_no_CBCL_striatal_feed_outliers_males_prepubertal$src_subject_id) #1515

# No CBCL or OFC anticipation outliers.
data_no_CBCL_lOFC_ant_outliers <- subset(PDS_correct, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_lOFC_ant_outliers) # 4061.

data_no_CBCL_lOFC_ant_outliers_females <- subset(PDS_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_lOFC_ant_outliers_females) # 1992.

data_no_CBCL_lOFC_ant_outliers_males <- subset(PDS_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_lOFC_ant_outliers_males) # 2069.

data_no_CBCL_lOFC_ant_outliers_females_pubertal <- subset(PDS_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
length(data_no_CBCL_lOFC_ant_outliers_females_pubertal$src_subject_id) #1373
data_no_CBCL_lOFC_ant_outliers_males_pubertal <- subset(PDS_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
length(data_no_CBCL_lOFC_ant_outliers_males_pubertal$src_subject_id) #568

data_no_CBCL_lOFC_ant_outliers_females_prepubertal <- subset(PDS_correct_females, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
length(data_no_CBCL_lOFC_ant_outliers_females_prepubertal$src_subject_id) #619
data_no_CBCL_lOFC_ant_outliers_males_prepubertal <- subset(PDS_correct_males, lOFC_rvsn_ant_z > -3 & lOFC_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
length(data_no_CBCL_lOFC_ant_outliers_males_prepubertal$src_subject_id) #1501


# No CBCL or OFC FEEDBACK outliers.
data_no_CBCL_lOFC_feed_outliers <- subset(PDS_correct, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_lOFC_feed_outliers) #4086

data_no_CBCL_lOFC_feed_outliers_females <- subset(PDS_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_lOFC_feed_outliers_females) # 1999

data_no_CBCL_lOFC_feed_outliers_males <- subset(PDS_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)
nrow(data_no_CBCL_lOFC_feed_outliers_males) # 2087.

data_no_CBCL_lOFC_feed_outliers_females_pubertal <- subset(PDS_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category != 1)
length(data_no_CBCL_lOFC_feed_outliers_females_pubertal$src_subject_id) #1379
data_no_CBCL_lOFC_feed_outliers_males_pubertal <- subset(PDS_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category != 1)
length(data_no_CBCL_lOFC_feed_outliers_males_pubertal$src_subject_id) #579

data_no_CBCL_lOFC_feed_outliers_females_prepubertal <- subset(PDS_correct_females, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 & pds_p_ss_category == 1)
length(data_no_CBCL_lOFC_feed_outliers_females_prepubertal$src_subject_id) #620
data_no_CBCL_lOFC_feed_outliers_males_prepubertal <- subset(PDS_correct_males, lOFC_posvsneg_feedback_z > -3 & lOFC_posvsneg_feedback_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3 &  pds_p_ss_category == 1)
length(data_no_CBCL_lOFC_feed_outliers_males_prepubertal$src_subject_id) #1508


# No CBCL or testosterone outliers.
data_no_CBCL_test_outliers <- subset(PDS_correct, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)

data_no_CBCL_test_outliers_females <- subset(PDS_correct_females, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)

data_no_CBCL_test_outliers_males <- subset(PDS_correct_males, hormone_scr_ert_mean_z > -3 & hormone_scr_ert_mean_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3)

# Get z scores for bis bas rr scores.
mean_bisbas <- mean(PDS_correct$bisbas_ss_basm_rr, na.rm=TRUE)
sd_bisbas <- sd(PDS_correct$bisbas_ss_basm_rr, na.rm=TRUE)
PDS_correct$bisbas_ss_basm_rr_z <- (PDS_correct$bisbas_ss_basm_rr-mean_bisbas)/sd_bisbas

data_no_bisbas_outliers <- subset(PDS_correct, bisbas_ss_basm_rr_z > -3 & bisbas_ss_basm_rr_z < 3 )
nrow(data_no_bisbas_outliers) # 4204.
data_no_bisbas_outliers_females <- subset(data_no_bisbas_outliers, sex == "F")
nrow(data_no_bisbas_outliers_females) # 2049.
data_no_bisbas_outliers_males <- subset(data_no_bisbas_outliers, sex == "M")
nrow(data_no_bisbas_outliers_males) # 2155.
