# This script calculates the averages to use for the MRI data.
# It averages the right and left hemisphere values and also creates a composite striatum value by averaging the accumbens, caudate, and putamen.
# Because we have already split the data, I am passing in the exploratory data set.
# This step could be moved earlier, before splitting the data, in which case you should pass in the whole data set.

library(ggplot2)
library(gamm4)
library(dplyr)
library(sjPlot)

# Change this to your directory and the name of your data file.
datapath <- "/Users/nataliesaragosa-harris/Desktop/ABCD/output"
datapath <- "C:/Users/lucia/GitHub/ABCD/derivatives/exploratory/"


datafile <- "nda20_exploratory.csv"
setwd(datapath)

data <- read.table(datafile, header=T, sep=",",as.is=TRUE,strip.white=TRUE,fill=TRUE)
length(data$src_subject_id) # 5934.

# "tfmri_ma_acdn_b_scs_aarh", # Accumbens reward vs. neutral anticipation.
# "tfmri_ma_acdn_b_scs_aalh",
# "tfmri_ma_acdn_b_scs_cdrh", # Caudate reward vs. neutral anticipation.
# "tfmri_ma_acdn_b_scs_cdlh",
# "tfmri_ma_acdn_b_scs_ptrh", # Putamen reward vs. neutral anticipation.
# "tfmri_ma_acdn_b_scs_ptlh",
# "tfmri_ma_arvn_b_cds_mobofrrh", # Medial OFC reward vs. neutral anticipation.
# "tfmri_ma_arvn_b_cds_mobofrlh",
# "tfmri_ma_arvn_b_cds_lobofrrh", # Lateral OFC reward vs. neutral anticipation.
# "tfmri_ma_arvn_b_cds_lobofrlh",
# "tfmri_ma_rpvnfb_b_scs_aarh", # Accumbens reward positive versus negative feedback.
# "tfmri_ma_rpvnfb_b_scs_aalh",
# "tfmri_ma_rpvnfb_b_scs_cdrh", # Caudate reward positive versus negative feedback.
# "tfmri_ma_rpvnfb_b_scs_cdlh",
# "tfmri_ma_rpvnfb_b_scs_ptrh", # Putamen reward positive versus negative feedback.
# "tfmri_ma_rpvnfb_b_scs_ptlh",
# "tfmri_ma_rpvnfb_b_cds_mobofrrh", # Medial OFC reward positive versus negative feedback.
# "tfmri_ma_rpvnfb_b_cds_mobofrlh",
# "tfmri_ma_rpvnfb_b_cds_lobofrrh", # Lateral OFC reward positive versus negative feedback.
# "tfmri_ma_rpvnfb_b_cds_lobofrlh"

# Average right and left hemispheres.
# Note: If either is "NA", then the whole thing will be "NA".
data$accumbens_rvsn_ant <- (data$tfmri_ma_acdn_b_scs_aarh + data$tfmri_ma_acdn_b_scs_aalh)/2
data$caudate_rvsn_ant <- (data$tfmri_ma_acdn_b_scs_cdrh + data$tfmri_ma_acdn_b_scs_cdlh)/2
data$putamen_rvsn_ant <- (data$tfmri_ma_acdn_b_scs_ptrh + data$tfmri_ma_acdn_b_scs_ptlh)/2
data$mOFC_rvsn_ant <- (data$tfmri_ma_arvn_b_cds_mobofrrh + data$tfmri_ma_arvn_b_cds_mobofrlh)/2
data$lOFC_rvsn_ant <- (data$tfmri_ma_arvn_b_cds_lobofrrh + data$tfmri_ma_arvn_b_cds_lobofrlh)/2

# Do the same for feedback stage as well.
data$accumbens_posvsneg_feedback <- (data$tfmri_ma_rpvnfb_b_scs_aarh + data$tfmri_ma_rpvnfb_b_scs_aalh)/2
data$caudate_posvsneg_feedback <- (data$tfmri_ma_rpvnfb_b_scs_cdrh + data$tfmri_ma_rpvnfb_b_scs_cdlh)/2
data$putamen_posvsneg_feedback <- (data$tfmri_ma_rpvnfb_b_scs_ptrh + data$tfmri_ma_rpvnfb_b_scs_ptlh)/2
data$mOFC_posvsneg_feedback <- (data$tfmri_ma_rpvnfb_b_cds_mobofrrh + data$tfmri_ma_rpvnfb_b_cds_mobofrlh)/2
data$lOFC_posvsneg_feedback <- (data$tfmri_ma_rpvnfb_b_cds_lobofrrh + data$tfmri_ma_rpvnfb_b_cds_lobofrlh)/2

 
data$striatum_rvsn_ant <- (data$accumbens_rvsn_ant + 
                             data$caudate_rvsn_ant+
                             data$putamen_rvsn_ant)/3
# striatum_rvsn_ant = reward vs. neutral anticipation.

data$striatum_posvsneg_feedback <- (data$accumbens_posvsneg_feedback +
                                      data$caudate_posvsneg_feedback +
                                      data$putamen_posvsneg_feedback)/3


# Calculate z scores for all of these values.
data$accumbens_rvsn_ant_z <- as.numeric(scale(data$accumbens_rvsn_ant))
data$caudate_rvsn_ant_z <- scale(data$caudate_rvsn_ant)
data$putamen_rvsn_ant_z <- scale(data$putamen_rvsn_ant)
data$mOFC_rvsn_ant_z <- scale(data$mOFC_rvsn_ant)
data$lOFC_rvsn_ant_z <- scale(data$lOFC_rvsn_ant)

data$accumbens_posvsneg_feedback_z <- scale(data$accumbens_posvsneg_feedback)
data$caudate_posvsneg_feedback_z <- scale(data$caudate_posvsneg_feedback)
data$putamen_posvsneg_feedback_z <- scale(data$putamen_posvsneg_feedback)
data$mOFC_posvsneg_feedback_z <- scale(data$mOFC_posvsneg_feedback)
data$lOFC_posvsneg_feedback_z <- scale(data$lOFC_posvsneg_feedback)

data$striatum_rvsn_ant_z <- scale(data$striatum_rvsn_ant)
data$striatum_posvsneg_feedback_z <- scale(data$striatum_posvsneg_feedback)


# striatum_posvsneg_feedback = reward positive vs. negative feedback.
setwd(datapath)
write.csv(data,"nda20_exploratory.csv", row.names = FALSE)
