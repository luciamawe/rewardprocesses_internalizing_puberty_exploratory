require(dplyr)


data_dir = ((dirname(here()))) #figure out how to navigate to data source
data_folder <- file.path(data_dir,"ABCD","derivatives","exploratory")  
file_name <- "nda30_exploratory.csv" #specify file name here 
output_directory <- data_folder

nda30 <- read.table(file.path(data_folder,file_name),header=T, sep=",",as.is=TRUE,strip.white=TRUE,fill=TRUE)

df <- nda30
df <- df %>% filter(eventname=="baseline_year_1_arm_1")

##Check to see if 'sex' and 'hormone_sal_sex' data matches.
###Exploratory
#5 Female with misclassified Male tubes. 6 Male with misclassified Female tubes. 49 either had issues at saliva collection or had NA gender values.
#Let's get rid of them. We go down from 5691 -> 5905 (-56)

table(df$sex, df$hormone_sal_sex)

df  <- df[-c(which(df$sex == "M" & df$hormone_sal_sex == 1), 
             which(df$sex =="F" & df$hormone_hormone_sal_sex == 2),
             which(df$hormone_sal_sex == 3),
             which(df$hormone_sal_sex == 4),
             which(df$hormone_sal_sex == 5),
             which(is.na(df$sex)),
             which(is.na(df$hormone_sal_sex))),]


###################### NDA --- DEAP
# hormone_sal_sex       1       Pink (female)
# hormone_sal_sex       2       Blue (male)
# hormone_sal_sex       3       Participant unable to complete
# hormone_sal_sex       4       Participant/Parent refused
# hormone_sal_sex       5       Not collected (other)      

df$hormon_sal_notes_y___1

#Let's filter the data
#The filter scheme is to check records for any RA saliva collection notes. If true, then flag the record. 
#Then check flagged records and see if the Salimetrics value is out of range per hormone.
#If yes, then change value to NA, else keep the existing values for each replicate.
#Finally, average the two replicates into a new field.
df$hormone_notes_ss <- as.numeric(df$hormon_sal_notes_y___2) + 
                       as.numeric(df$hormon_sal_notes_y___3) +
                       as.numeric(df$hormon_sal_notes_y___4) + 
                       as.numeric(df$hormon_sal_notes_y___5) + 
                       as.numeric(df$hormon_sal_notes_y___6)
rownums <- which(df$hormone_notes_ss > 1)

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

boxplot(df$filtered_testosterone)
boxplot(df$hormone_scr_ert_mean)

boxplot(df$hormone_sal_sex,df$filtered_testosterone, ylim = c(0,50))
boxplot(df$hormone_scr_ert_mean~df$sex,ylim = c(0,50))


boxplot(df$filtered_testosterone~df$sex,ylim = c(0,600))

summary(df$filtered_testosterone)
summary(df$hormone_scr_ert_mean)

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