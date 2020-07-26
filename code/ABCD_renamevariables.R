# Written by Natalie Saragosa-Harris.
# June 2020.
# This code renames the ABCD variables to have shorter and more interpretable names.

library(plyr)
# Replace with your directories here.
variabledirectory <- "/Users/nataliesaragosa-harris/Desktop/ABCD"
datadirectory <- "/Users/nataliesaragosa-harris/Desktop/ABCD/output"

setwd(variabledirectory)
variablenames <- read.csv("VariableDefinitions.csv")
setwd(datadirectory)
datafile <- readRDS('nda20.rds')

variables <- as.data.frame(variablenames$Variable.Name)
variables <- variables[!apply(variables == "", 1, all),] # Remove empty rows.
variables <- tolower(variables)  # Make sure they are all lowercase.
missingvariables <- setdiff(variables,colnames(datafile)) # List variables that are in the "variable names" file but not in the data file.
missingvariables <- as.data.frame(missingvariables)
colnames(missingvariables) <- "Variable.Name"


print("These variable names do not appear in the data: ")
print(missingvariables)

# Get the aliases for all of the variable names.
aliases <- variablenames[,c("Variable.Name","Aliases")]
aliases <- aliases[!apply(aliases == "", 1, all),] # Remove empty rows.
aliases$Variable.Name <- tolower(aliases$Variable.Name)  # Make sure they are all lowercase.
aliases$Aliases <- tolower(aliases$Aliases) 
aliases <- as.data.frame(aliases)


missingvariables <- merge(missingvariables,aliases, by = "Variable.Name")
# Replace with aliases.
# For the variables that are not in the data frame, check if the alias exists in the data frame.

no_aliases <- subset(missingvariables, Aliases== "")
print("No alias provided for the following variables: ")
print(no_aliases$Variable.Name)


# Keep only the ones that have an alias.
missingvariables <- subset(missingvariables, Aliases!= "")

# Do these aliases exist in the dataframe?
aliases_in_data <- intersect(missingvariables$Aliases,colnames(datafile))
print("Alias exists in dataframe for the following variables: ")
print(aliases_in_data)

aliases_not_in_data <- setdiff(missingvariables$Aliases,colnames(datafile))
print("Neither the original variable name nor its alias exist in the dataframe for the following variables: ")
print(aliases_not_in_data)


# Go through the variables that do have aliases in the data.
for (i in 1:length(aliases_in_data)){
  
  # Find that alias in the dataframe and in the aliases dataframe.
  # Rename the column from the alias to the correct Variable Name.
  alias <- aliases_in_data[i]
  # Find alias and the original variable name in "aliases" data frame.
  new_name <- subset(aliases, Aliases == alias)
  new_name <- new_name$Variable.Name
  # Find alias in dataframe and replace with the new name.
  names(datafile)[names(datafile) == alias] <- new_name
}

print("The following variables were not found in the data frame under this name or an alias: ")
print(setdiff(variables,colnames(datafile)))

# We need to calculate PDS category ourselves so these two variables are not in the dataframe (see definition in variable definitions file).
# pds_p_ss_male_category.
# pds_p_ss_female_category.

# The bisbas ones need to be calculated (see definition in variable definitions file).
# bisbas_ss_basm_rr.
# bisbas_ss_basm_rr_nm.

# The hormone ones are not included.
hormone_sal_wake_y
hormone_sal_caff_y
imgincl_mid_include

# Save the dataframe with the new variable names.
setwd(datadirectory)
# save RDS file.
# save csv.

############## Delete this section later. ##############

# The interview_age variable is duplicated.
(setdiff(datafile$interview_age.x,datafile$interview_age.y))
differences <- subset(datafile, interview_age.x != interview_age.y)
differences <- datafile[,c("src_subject_id","interview_age.x","interview_age.y")]
differences <- subset(differences, !is.na(interview_age.y))
(setdiff(differences$interview_age.x,differences$interview_age.y)) # Now there are no differences.

# The sex variable is duplicated.
(setdiff(datafile$sex.x,datafile$sex.y)) # No differences so they are the same.

########################################################
