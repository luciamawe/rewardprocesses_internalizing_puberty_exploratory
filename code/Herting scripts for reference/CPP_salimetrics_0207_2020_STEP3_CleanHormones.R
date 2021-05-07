require(dplyr)

df <- tbl_df(readRDS('~/Downloads/ABCD_release2.0.1_update_Rds/nda2.0.1.Rds'))
df <- df %>% filter(eventname=="baseline_year_1_arm_1")

##Check to see if 'gender' and 'biospec_hormone_sal_sex' data matches.
#21 Female with misclassified Male tubes. 12 Male with misclassified Female tubes. 81 either had issues at saliva collection or had NA gender values.
#Let's get rid of them. We go down from 11875 -> 11761 (-114)
table(df$sex, df$biospec_hormone_sal_sex)
df  <- df[-c(which(df$sex == "M" & df$biospec_hormone_sal_sex == "Pink/Purple (female)"), 
             which(df$sex =="F" & df$biospec_hormone_sal_sex == "Blue (male)"),
             which(df$biospec_hormone_sal_sex == "Participant unable to complete"),
             which(df$biospec_hormone_sal_sex == "Participant/Parent refused"),
             which(df$biospec_hormone_sal_sex == "Not collected (other)"),
             which(is.na(df$sex)),
             which(is.na(df$biospec_hormone_sal_sex))),]


#Let's filter the data
#The filter scheme is to check records for any RA saliva collection notes. If true, then flag the record. 
#Then check flagged records and see if the Salimetrics value is out of range per hormone.
#If yes, then change value to NA, else keep the existing values for each replicate.
#Finally, average the two replicates into a new field.
df$hormone_notes_ss <- as.numeric(df$biospec_hormon_sal_notes___2) + 
                       as.numeric(df$biospec_hormon_sal_notes___3) +
                       as.numeric(df$biospec_hormon_sal_notes___4) + 
                       as.numeric(df$biospec_hormon_sal_notes___5) + 
                       as.numeric(df$biospec_hormon_sal_notes___6)
rownums <- which(df$hormone_notes_ss > 5)

#DHEA
df$filtered_dhea <- NA
df$filtered_dhea_rep1 <- df$hormone_scr_dhea_rep1
df$filtered_dhea_rep2 <- df$hormone_scr_dhea_rep2
df$filtered_dhea[which(df$hormone_scr_dhea_rep1_nd == 1)] <- 0
df$filtered_dhea[which(df$hormone_scr_dhea_rep2_nd == 1)] <- 0
rownums_rep1 <- which(df$hormone_scr_dhea_rep1 < 5 | df$hormone_scr_dhea_rep1 > 1000)
rownums_rep2 <- which(df$hormone_scr_dhea_rep2 < 5 | df$hormone_scr_dhea_rep2 > 1000)
df$filtered_dhea_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA
df$filtered_dhea_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA
df$filtered_dhea <- apply(df[, c("filtered_dhea_rep1", "filtered_dhea_rep2")], 1, function(x) mean(x, na.rm=T))


#Testosterone
df$filtered_testosterone <- NA
df$filtered_testosterone_rep1 <- df$hormone_scr_ert_rep1
df$filtered_testosterone_rep2 <- df$hormone_scr_ert_rep2
df$filtered_testosterone[which(df$hormone_scr_ert_rep1_nd == 1)] <- 0
df$filtered_testosterone[which(df$hormone_scr_ert_rep2_nd == 1)] <- 0
rownums_rep1 <- which(df$hormone_scr_ert_rep1 < 1 | df$hormone_scr_ert_rep1 > 600)
rownums_rep2 <- which(df$hormone_scr_ert_rep2 < 1 | df$hormone_scr_ert_rep2 > 600)
df$filtered_testosterone_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA
df$filtered_testosterone_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA
df$filtered_testosterone <- apply(df[, c("filtered_testosterone_rep1", "filtered_testosterone_rep2")], 1, function(x) mean(x, na.rm=T))


#Estradiol
df$filtered_estradiol <- NA
df$filtered_estradiol_rep1 <- df$hormone_scr_hse_rep1
df$filtered_estradiol_rep2 <- df$hormone_scr_hse_rep2
df$filtered_estradiol[which(df$hormone_scr_hse_rep1_nd == 1)] <- 0
df$filtered_estradiol[which(df$hormone_scr_hse_rep2_nd == 1)] <- 0
rownums_rep1 <- which(df$hormone_scr_hse_rep1 < 0.1 | df$hormone_scr_hse_rep1 > 32)
rownums_rep2 <- which(df$hormone_scr_hse_rep2 < 0.1 | df$hormone_scr_hse_rep2 > 32)
df$filtered_estradiol_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA
df$filtered_estradiol_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA
df$filtered_estradiol <- apply(df[, c("filtered_estradiol_rep1", "filtered_estradiol_rep2")], 1, function(x) mean(x, na.rm=T))



df <- df %>% select("src_subject_id", "eventname", "filtered_dhea", "filtered_estradiol", "filtered_testosterone", 
                    "biospec_hormone_sal_sex", "sex")
saveRDS(df,"SalimetricsClean_eric.RDS")