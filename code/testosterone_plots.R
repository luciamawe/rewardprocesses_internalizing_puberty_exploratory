# Plotting testosterone values.

library(ggplot2)
library(here)
library(glue)
library(dplyr)

data_dir = ((dirname(here()))) 

# First, let's load the exploratory data and assign unique names to the dataframes we will use for plotting results from the exploratory data.
# When call the define_dataframes script, will keep only the baseline data.

# First, let's load the exploratory data and assign unique names to the dataframes we will use for plotting results from the exploratory data.
phase_folder = "exploratory"  # select the appropriate folder.
data_folder <- file.path(data_dir,"ABCD","derivatives",phase_folder)
file_name <- glue("nda30_{phase_folder}.csv")
fulldata <- read.csv(file.path(data_folder,file_name))
nrow(fulldata) # 27321.

# Call function to define all of the dataframes.
script_path <- file.path(data_dir,"rewardprocesses_internalizing_puberty_exploratory","code","define_dataframes.R")
source(script_path)

# Use PDS_correct as the dataframe.
sample1 <- PDS_correct
nrow(sample1) #5631.

# Do the same process, but with the confirmatory sample.
phase_folder = "confirmatory"  # select the appropriate folder.
data_folder <- file.path(data_dir,"ABCD","derivatives",phase_folder)
file_name <- glue("nda30_{phase_folder}.csv")
fulldata <- read.csv(file.path(data_folder,file_name))
nrow(fulldata) #  27331 (confirmatory).

# Call function to define all of the dataframes.
script_path <- file.path(data_dir,"rewardprocesses_internalizing_puberty_exploratory","code","define_dataframes.R")
source(script_path)

# Use PDS_correct as the dataframe.
sample2 <- PDS_correct
nrow(sample2) #5612


nsh_theme <- theme(text = element_text(family = "Avenir"),        
                   title= element_text(size=26, vjust=2, face="bold"),
                   #plot.title = element_blank(),
                   plot.subtitle = element_text(color="gray40", size=18, face="bold.italic"),
                   axis.title.x= element_text(size=24, vjust=-0.3),
                   axis.title.y= element_text(size=24, vjust=1.5),
                   axis.text.x= element_text(size=28, colour="black"),
                   axis.text.y= element_text(size=28, colour="black"),
                   strip.text = element_text(size=24, face="bold"),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


sample1 <- sample1 %>% mutate(is_testosterone_outlier = case_when(hormone_scr_ert_mean_z > 3 ~ 1,
                                                 hormone_scr_ert_mean_z < -3 ~ 1, 0))


sample1_testosterone <- sample1 %>%
  select(sex,hormone_scr_ert_mean,hormone_scr_ert_mean_z) %>%
  mutate(
    is_testosterone_outlier = case_when(
      hormone_scr_ert_mean_z < -3 | hormone_scr_ert_mean_z > 3 ~ "yes",
      TRUE                      ~  "no"
    )
  )

sample1_testosterone$sample <- "sample 1"

sample2_testosterone <- sample2 %>%
  select(sex,hormone_scr_ert_mean,hormone_scr_ert_mean_z) %>%
  mutate(
    is_testosterone_outlier = case_when(
      hormone_scr_ert_mean_z < -3 | hormone_scr_ert_mean_z > 3 ~ "yes",
      TRUE                      ~  "no"
    )
  )

sample2_testosterone$sample <- "sample 2"



sample1_outliers <- subset(sample1_testosterone,is_testosterone_outlier == "yes")
sample1_outliers_cutoff <- sample1_outliers[which(sample1_outliers$hormone_scr_ert_mean_z == min(sample1_outliers$hormone_scr_ert_mean_z)),]
# Now save the value (in pg/ml) to plot.
sample1_outliers_cutoff <- sample1_outliers_cutoff$hormone_scr_ert_mean # 94.91.                     

sample2_outliers <- subset(sample2_testosterone,is_testosterone_outlier == "yes")
sample2_outliers_cutoff <- sample2_outliers[which(sample2_outliers$hormone_scr_ert_mean_z == min(sample2_outliers$hormone_scr_ert_mean_z)),]
# Now save the value (in pg/ml) to plot.
sample2_outliers_cutoff <- sample2_outliers_cutoff$hormone_scr_ert_mean #88.5795.                     


#testosterone_to_plot <- rbind(sample1_testosterone,sample2_testosterone)

# Testosterone distributions in both samples.

# Sample 1.
ggplot(sample1_testosterone, aes(x=hormone_scr_ert_mean)) + 
  geom_density(alpha=0.86) +
  geom_vline(aes(xintercept=sample1_outliers_cutoff), color = "black", # Add line to show where lowest value of outlier is.
           linetype="dashed", size=1) +
  labs(title = "Distribution of Testosterone Scores",
       subtitle = "Sample 1") +
  xlab("Testosterone value (pg/ml)") + 
  ylab("Density") +
  nsh_theme


# Sample 2.
ggplot(sample2_testosterone, aes(x=hormone_scr_ert_mean)) + 
  geom_density(alpha=0.86) +
  geom_vline(aes(xintercept=sample2_outliers_cutoff), color = "black", # Add line to show where lowest value of outlier is.
             linetype="dashed", size=1) +
  labs(title = "Distribution of Testosterone Scores",
       subtitle = "Sample 2") +
  xlab("Testosterone value (pg/ml)") + 
  ylab("Density") +
  nsh_theme