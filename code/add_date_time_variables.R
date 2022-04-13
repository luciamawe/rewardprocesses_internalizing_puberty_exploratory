# Adding in MRI, menstruation, and hormone date and time variables.

library(dplyr)
library(here)
library(glue)

# Existing variables that are used in this script.
# iqc_mid_study_date: Date of first MID imaging series.
# iqc_mid_series_time: Time of first MID imaging series.
# hormone_sal_end_y: Hormone saliva sample time collection finished?
# menstrualcycle1_p: What was the date of the first day of your child's last period?
# interview_date_hormones [existing column that we rename in this script]: the 'interview_date' column specifically from hormone file (sph01.txt), renamed for clarity.

# Variables that are created in this script.
# iqc_mid_series_date_time [created]: iqc_mid_study_date + iqc_mid_series_time
# hormone_date_time [created]: interview_date_hormones + hormone_sal_end_y
# MRI_minus_hormone_date_time [created]:  iqc_mid_series_date_time - hormone_date_time [date/time variables]
# hormone_date_minus_last_period_date [created]: interview_date_hormones - menstrualcylcle1_p [dates only, no times added]

#data_dir = ((dirname(here()))) 
#exploratory_data <- read.csv(file.path(data_dir,"ABCD","derivatives","exploratory","nda30_exploratory.csv"))
#confirmatory_data <- read.csv(file.path(data_dir,"ABCD","derivatives","confirmatory","nda30_confirmatory.csv"))

data_dir = ((dirname(here()))) 

# Read in MRI date and time variables from mriqcrp302.csv.
mriqcrp302 <- read.csv(file.path(data_dir,"ABCD","individualdatafiles","mriqcrp302.csv"), header = TRUE)
mriqcrp302 = mriqcrp302[-1,] # Remove first row (variable definitions).
# Add date and time columns to create new column.
mriqcrp302$iqc_mid_series_date_time <- paste(mriqcrp302$iqc_mid_study_date, mriqcrp302$iqc_mid_series_time, sep = " ")

# Read in sph01 file, rename interview_date as “interview_date_hormones”.
# Can ignore the rest of the columns from sph01 since we already merged those.
sph01 <- read.csv(file.path(data_dir,"ABCD","individualdatafiles","sph01.csv"), header = TRUE)
#In sph01, 'eventname' is called 'visit', so need to change that first.
sph01 <- dplyr::rename(sph01, "eventname" = "visit")
# Now rename "interview_date" to be "interview_date_hormones".
sph01 <- dplyr::rename(sph01, "interview_date_hormones" = "interview_date")
sph01 <- sph01[,c("src_subject_id","eventname","interview_date_hormones")]

# Read in existing data file.
phase_folder = "exploratory"  # select the appropriate folder.
#phase_folder = "confirmatory"
data_folder <- file.path(data_dir,"ABCD","derivatives",phase_folder)
file_name <- glue("nda30_{phase_folder}.csv") # specify file name here. 
fulldata <- read.csv(file.path(data_folder,file_name))
nrow(fulldata) # 27321 (exploratory). When call the define_dataframes script, will keep only the baseline data.

# Merge MRI data with fulldata.
duplicate_columns <- c("collection_id", "collection_title", "subjectkey", "interview_age","interview_date","sex","dataset_id")
mriqcrp302 = mriqcrp302[,!(names(mriqcrp302) %in% duplicate_columns)] # Ignore duplicate columns for merging purposes.
fulldata <- merge(fulldata, mriqcrp302,by=c("src_subject_id","eventname"),all.x =TRUE)

# Merge sph01 data with fulldata (should just be adding the "interview_date_hormones" column).
fulldata <- merge(fulldata,sph01,by=c("src_subject_id","eventname"),all.x =TRUE)
#dim(fulldata) # 27321 (exploratory). When call the define_dataframes script, will keep only the baseline data.
#str(fulldata$iqc_mid_series_date_time)

# Create a variable that takes into account both date and time for hormone collection.
fulldata$hormone_date_time <- paste(fulldata$interview_date_hormones, fulldata$hormone_sal_end_y, sep = " ")

# Make sure time columns are in correct format. I don't know the time zone but this should not differ within person so it should be fine.
# For the columns with times, I am going to just do "GMT", which is UTC (Universal Time, Coordinated), for everyone so that they don't somehow get two different time zones for the same person.
fulldata$iqc_mid_series_date_time <- as.POSIXlt(fulldata$iqc_mid_series_date_time, format = "%Y-%m-%d %H:%M", "GMT")
fulldata$hormone_date_time <- as.POSIXlt(fulldata$hormone_date_time, format = "%m/%d/%Y %H:%M", "GMT")
fulldata$interview_date_hormones <- as.POSIXlt(fulldata$interview_date_hormones, format = "%m/%d/%Y", "GMT")
fulldata$menstrualcycle1_p <- as.POSIXlt(fulldata$menstrualcycle1_p, format = "%m/%d/%y", "GMT")

# Create a variable that takes into account the time difference (in minutes) between the MID task and hormone collection (using both date and time, as the dates may differ)
fulldata$MRI_minus_hormone_date_time <- fulldata$iqc_mid_series_date_time - fulldata$hormone_date_time
# It is in "difftime" format, so make it numeric.
fulldata$MRI_minus_hormone_date_time <- as.numeric(fulldata$MRI_minus_hormone_date_time)

# Remove the people who reports last period being in 2007, 2009, and 2012 (I think these are errors). 
range(fulldata$menstrualcycle1_p,na.rm=TRUE)
fulldata$menstrualcycle1_p[which(fulldata$menstrualcycle1_p ==  as.POSIXlt("2007-05-28","GMT"))] <- NA
fulldata$menstrualcycle1_p[which(fulldata$menstrualcycle1_p ==  as.POSIXlt("2009-08-18","GMT"))] <- NA
fulldata$menstrualcycle1_p[which(fulldata$menstrualcycle1_p ==  as.POSIXlt("2012-01-01","GMT"))] <- NA


# Create a variable that indicates, as of day of hormone collection, days since last period (using dates only).
fulldata$hormone_date_minus_last_period_date <- fulldata$interview_date_hormones - fulldata$menstrualcycle1_p
# For some reason it puts it in seconds, so let's divide by to put it into days.
# It is in "difftime" format, so make it numeric.
# For some reason it defaults to put it in seconds, so let's divide by to put it into days.
fulldata$hormone_date_minus_last_period_date <- as.numeric(fulldata$hormone_date_minus_last_period_date)
fulldata$hormone_date_minus_last_period_date <- fulldata$hormone_date_minus_last_period_date/86400
range(fulldata$hormone_date_minus_last_period_date,na.rm=TRUE)

#fulldata$interview_date_hormones[which(fulldata$hormone_date_minus_last_period_date==115948800)] #  "2019-04-20 GMT"
#fulldata$menstrualcycle1_p[which(fulldata$hormone_date_minus_last_period_date==115948800)] # "2015-08-17 GMT"

# sum(!is.na(fulldata$iqc_mid_series_date_time)) # 8602 (exploratory).

# baseline <- subset(fulldata,eventname == "baseline_year_1_arm_1")
# range(baseline$hormone_date_minus_last_period_date,na.rm=TRUE) # Range is 0 474.
# baseline$interview_date_hormones[which(baseline$hormone_date_minus_last_period_date==474)] # "2017-04-29 GMT"
# baseline$menstrualcycle1_p[which(baseline$hormone_date_minus_last_period_date==474)] # "2016-01-11 GMT"
# So while one person has a very large value, it is possible that they had not had their period for over a year at the time of data collection.

# range(baseline$MRI_minus_hormone_date_time,na.rm=TRUE) # Range is -1159 (-19 hours) and 98051 (68 days).
