# Plotting testosterone values.

library(ggplot2)
library(here)
library(glue)
library(dplyr)
library(patchwork)

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
nrow(sample1) # 5626.

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
nrow(sample2) #5600.

nsh_theme <- theme(text = element_text(family = "Avenir"),        
                   #title= element_text(size=20, vjust=2, face="bold"),
                   plot.title = element_blank(),
                   plot.subtitle = element_text(color="gray40", size=20, face="bold.italic"),
                   axis.title.x= element_text(size=20, vjust=-0.3),
                   axis.title.y= element_text(size=20, vjust=1.5),
                   axis.text.x= element_text(size=20, colour="black"),
                   axis.text.y= element_text(size=20, colour="black"),
                   strip.text = element_text(size=20, face="bold"),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))


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


sample1_outliers_females <- subset(sample1_testosterone,is_testosterone_outlier == "yes" & sex == "F")
sample1_outliers_females_cutoff <- sample1_outliers_females[which(sample1_outliers_females$hormone_scr_ert_mean_z == min(sample1_outliers_females$hormone_scr_ert_mean_z)),]
sample1_outliers_females_cutoff <- sample1_outliers_females_cutoff$hormone_scr_ert_mean # Now save the value (in pg/ml) to plot (94.91).                     
# mean(sample1_outliers_females$hormone_scr_ert_mean)

sample1_outliers_males <- subset(sample1_testosterone,is_testosterone_outlier == "yes" & sex == "M")
sample1_outliers_males_cutoff <- sample1_outliers_males[which(sample1_outliers_males$hormone_scr_ert_mean_z == min(sample1_outliers_males$hormone_scr_ert_mean_z)),]
sample1_outliers_males_cutoff <- sample1_outliers_males_cutoff$hormone_scr_ert_mean # Now save the value (in pg/ml) to plot (97.61).              

sample2_outliers_females <- subset(sample2_testosterone,is_testosterone_outlier == "yes" & sex == "F")
sample2_outliers_females_cutoff <- sample2_outliers_females[which(sample2_outliers_females$hormone_scr_ert_mean_z == min(sample2_outliers_females$hormone_scr_ert_mean_z)),]
sample2_outliers_females_cutoff <- sample2_outliers_females_cutoff$hormone_scr_ert_mean # Now save the value (in pg/ml) to plot (88.96).                     

sample2_outliers_males <- subset(sample2_testosterone,is_testosterone_outlier == "yes" & sex == "M")
sample2_outliers_males_cutoff <- sample2_outliers_males[which(sample2_outliers_males$hormone_scr_ert_mean_z == min(sample2_outliers_males$hormone_scr_ert_mean_z)),]
sample2_outliers_males_cutoff <- sample2_outliers_males_cutoff$hormone_scr_ert_mean # Now save the value (in pg/ml) to plot (88.5795).


# Testosterone distributions in both samples.

# Sample 1.
females_sample1_plot <- ggplot(subset(sample1_testosterone, sex == "F"), aes(x=hormone_scr_ert_mean)) + 
  geom_density(alpha=0.86, color = "aquamarine4", size = 2) +
  geom_vline(aes(xintercept = sample1_outliers_females_cutoff), color = "black", # Add line to show where lowest value of outlier is.
           linetype="dashed", size=1) +
  labs(subtitle = "Female participants") +
  xlab("Testosterone value (pg/ml)") + 
  ylab("Density") +
  #scale_x_continuous(limits = c(0, 600), breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = c(seq(0,600,100))) +
  coord_cartesian(xlim = c(0,600)) + 
  nsh_theme

males_sample1_plot <- ggplot(subset(sample1_testosterone, sex == "M"), aes(x=hormone_scr_ert_mean)) + 
  geom_density(alpha=0.86, color = "aquamarine4", size = 2) +
  geom_vline(aes(xintercept = sample1_outliers_males_cutoff), color = "black", # Add line to show where lowest value of outlier is.
             linetype="dashed", size=1) +
  labs(subtitle = "Male participants") +
  xlab("Testosterone value (pg/ml)") + 
  ylab("Density") +
  #scale_x_continuous(limits = c(0, 600), breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = c(seq(0,600,100))) +
  coord_cartesian(xlim = c(0,600)) + 
  nsh_theme

sample1_plot <- females_sample1_plot/males_sample1_plot + plot_annotation(title = "Distribution of Testosterone Scores: Sample 1",
                                                                          tag_levels = 'A',
                                                                          theme = theme(plot.title = element_text(size = 22),
                                                                                        text = element_text(family = "Avenir")))
sample1_plot

# Sample 2.
females_sample2_plot <- ggplot(subset(sample2_testosterone, sex == "F"), aes(x=hormone_scr_ert_mean)) + 
  geom_density(alpha=0.86, color = "aquamarine4", size = 2) +
  geom_vline(aes(xintercept = sample2_outliers_females_cutoff), color = "black", # Add line to show where lowest value of outlier is.
             linetype="dashed", size=1) +
  labs(subtitle = "Female participants") +
  xlab("Testosterone value (pg/ml)") + 
  ylab("Density") +
  #scale_x_continuous(limits = c(0, 600), breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = c(seq(0,600,100))) +
  coord_cartesian(xlim = c(0,600)) + 
  nsh_theme

males_sample2_plot <- ggplot(subset(sample2_testosterone, sex == "M"), aes(x=hormone_scr_ert_mean)) + 
  geom_density(alpha=0.86, color = "aquamarine4", size = 2) +
  geom_vline(aes(xintercept = sample2_outliers_males_cutoff), color = "black", # Add line to show where lowest value of outlier is.
             linetype="dashed", size=1) +
  labs(subtitle = "Male participants") +
  xlab("Testosterone value (pg/ml)") + 
  ylab("Density") +
  #scale_x_continuous(limits = c(0, 600), breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = c(seq(0,600,100))) +
  coord_cartesian(xlim = c(0,600)) + 
  nsh_theme

sample2_plot <- females_sample2_plot/males_sample2_plot + plot_annotation(title = "Distribution of Testosterone Scores: Sample 2",
                                                                          tag_levels = 'A',
                                                                          theme = theme(plot.title = element_text(size = 22),
                                                                                        text = element_text(family = "Avenir")))
sample2_plot
