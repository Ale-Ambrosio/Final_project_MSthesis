#Report script

list.of.packages <- c("devtools", "ggplot2", "tidyverse", "dplyr","xtable","stargazer","here",
                      "lubridate","gridExtra","skimr","tidyr","shiny","report","forcats",
                      "stringr", "purrr", "readr", "tibble", "patchwork")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Install ggcal separately if not present
ggcal_package = c('ggcal')
new.packages = ggcal_package[!(ggcal_package %in% installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_github("jayjacobs/ggcal")


library(here)
here() # we set the root directory as the main directory, all the paths will be now related to this
library(dplyr)

source(here("Reports","Report_scripts","Main_child.R"))

rm(proc_path)

#######Here we start the plotting process

# Define colors for each factor level
colors <- c("Green Label" = "#009E73", "Gray Label" = "#999999", "Red Label" = "#D55E00")

#we define the data_base
df_emotion = EM_all_long

rm(EM_all_long)

# Reorder the levels for the plots to show Green > Red > Gray
df_emotion$Label <- factor(df_emotion$Label, levels = c("Gray Label", "Red Label", "Green Label"))

# renaming columns

old_to_new_columns = c(
  'Freq_Worried/Anxious' = 'Categ_EMOTIE_Bang/angstig',
  'Freq_Irritated/Angry' = 'Categ_EMOTIE_Geïrriteerd/boos',
  'Freq_Stressed' = 'Categ_EMOTIE_Gestrest',
  'Freq_Relaxed/Calm' = 'Categ_EMOTIE_Ontspannen/kalm',
  'Freq_Happy' = 'Categ_EMOTIE_Blij/vrolijk',
  'Freq_Sad/Depressed' = 'Categ_EMOTIE_Droevig/somber',
  'Freq_Bored' = 'Categ_EMOTIE_Verveeld',
  'Freq_Tired' = 'Categ_EMOTIE_Vermoeid',
  'Freq_Energetic' = 'Categ_EMOTIE_Energiek',
  'Freq_Self-confident' = 'Categ_EMOTIE_Zelfverzekerd',
  'Freq_Insecure' = 'Categ_EMOTIE_Onzeker',
  'Freq_Ashamed/Guilty' = 'Categ_EMOTIE_Beschaamd/schuldig',
  'Freq_Lonely' = 'Categ_EMOTIE_Eenzaam',
  
  'Intens_Worried/Anxious' = 'Intensity_EMOTIE_Bang',
  'Intens_Irritated/Angry' = 'Intensity_EMOTIE_Geirriteerd',
  'Intens_Stressed' = 'Intensity_EMOTIE_Gestrest',
  'Intens_Relaxed/Calm' = 'Intensity_EMOTIE_Kalm',
  'Intens_Happy' = 'Intensity_EMOTIE_Blij',
  'Intens_Sad/Depressed' = 'Intensity_EMOTIE_Droevig',
  'Intens_Bored' = 'Intensity_EMOTIE_Verveeld',
  'Intens_Tired' = 'Intensity_EMOTIE_Vermoeid',
  'Intens_Energetic' = 'Intensity_EMOTIE_Energiek',
  'Intens_Self-confident' = 'Intensity_EMOTIE_Zelfverzekerd',
  'Intens_Insecure' = 'Intensity_EMOTIE_Onzeker',
  'Intens_Ashamed/Guilty' = 'Intensity_EMOTIE_Beschaamd',
  'Intens_Lonely' = 'Intensity_EMOTIE_Eenzam')

# now renaming

df_emotion = df_emotion %>% rename(old_to_new_columns)

###########################################################################
###########################################################################
###                                                                     ###
###                          MEDIAN DATAFRAMES                          ###
###                                                                     ###
###########################################################################
###########################################################################

#we define the two lists of pp numbers based on the median
group1 <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066", "EoA071", "EoA081", "EoA097", "EoA110", "EoA037", "EoA057", "EoA056", "EoA070", "EoA033", "EoA061", "EoA073", "EoA080", "EoA010", "EoA087", "EoA011", "EoA014", "EoA032", "EoA039", "EoA086", "EoA004", "EoA036", "EoA055", "EoA096", "EoA018", "EoA048", "EoA103", "EoA007", "EoA064", "EoA105", "EoA106", "EoA109")
group2 <- c("EoA030", "EoA044", "EoA045", "EoA077", "EoA102", "EoA022", "EoA023", "EoA052", "EoA091", "EoA002", "EoA012", "EoA015", "EoA042", "EoA078", "EoA083", "EoA092", "EoA104", "EoA008", "EoA031", "EoA035", "EoA038", "EoA069", "EoA043", "EoA067", "EoA075", "EoA024", "EoA063", "EoA089", "EoA100", "EoA090", "EoA114", "EoA034", "EoA108", "EoA006", "EoA085", "EoA115", "EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

#we create a new column with the group label
df_emotion <- df_emotion %>%
  mutate(Group = case_when(
    Participant.Label %in% group1 ~ "Low EE",
    Participant.Label %in% group2 ~ "High EE",
    TRUE ~ NA_character_  
  ))

# Define the allowed values
allowed_values <- c("Low EE", "High EE")

# Check if there are any disallowed values
if (any(!df_emotion$Group %in% allowed_values)) {
  stop("Error: The 'Group' column contains values other than 'Low EE' and 'High EE'. The error occurred at line 96 in the script Report_script.R")
}

rm(allowed_values)

#we calculate in a new dataframe the cumulative frequency of specific emotions per food label per participant
sum_per_subject <- df_emotion %>%
  group_by(Participant.Label, Label) %>%
  summarise(Freq_Stressed = sum(Freq_Stressed),
            Freq_Sad = sum(`Freq_Sad/Depressed`),
            Freq_Happy = sum(Freq_Happy),
            Freq_Bored = sum(Freq_Bored))

#we add the column Group to our sum_per_subject dataframe
# Select the relevant columns to join
group_info <- df_emotion %>%
  select(Participant.Label, Group) %>%
  distinct()

# Join the group information with the summarized dataframe
sum_per_subject <- sum_per_subject %>%
  left_join(group_info, by = "Participant.Label")

rm(group_info)

#we add EE scores to the dataframe
#we read the original file with EE scores 
EE_file = read.csv(here("EE_calculation", "Dataframes", "Output_data", "MEDIAN_R-processed_data_baseline.csv"), sep = ',')

#we only keep the columns of our interest
EE_file <- EE_file %>% select(EE_score, EoA)

#we remove pps that still have not finished EMA
pps_to_remove <- c("EoA116", "EoA117", "EoA118", "EoA119","EoA120", "EoA121", "EoA122", "EoA123", "EoA124", "EoA125", "EoA126", "EoA127", "EoA128", "EoA129", "Tessa_Test")
EE_file <- subset (EE_file, !(EoA %in% pps_to_remove))

#we bind the EE_scores to the sum_per_subject dataframe
EE_file <- EE_file %>%
  rename(Participant.Label = `EoA`)

sum_per_subject <- sum_per_subject %>%
  left_join(EE_file, by = "Participant.Label")

#we save this data-frame, that we will use to perform analyses in SPSS
# Define the file path
file_path_1 <- here("Reports","Report_output", "Dataframes", "Whole_sample", "For_ANOVA_frequency_whole.csv")

# Save the dataframe to the specified path
write.csv(sum_per_subject, file_path_1, row.names = FALSE)

rm(sum_per_subject, pps_to_remove, file_path_1)
########################################################################
#now we create a similar dataframe as before, but using the average instead of Intensities 
ave_intens_per_subject <- df_emotion %>%
  group_by(Participant.Label, Label) %>%
  summarise(Int_Stressed = mean(Intens_Stressed),
            Int_Sad = mean(`Intens_Sad/Depressed`),
            Int_Happy = mean(Intens_Happy),
            Int_Bored = mean(Intens_Bored))

#we add the column Group to our sum_per_subject dataframe
# Select the relevant columns to join
group_info <- df_emotion %>%
  select(Participant.Label, Group) %>%
  distinct()

# Join the group information with the summarized dataframe
ave_intens_per_subject <- ave_intens_per_subject %>%
  left_join(group_info, by = "Participant.Label")

rm(group_info)

#we add the EE scores
ave_intens_per_subject <- ave_intens_per_subject %>%
  left_join(EE_file, by = "Participant.Label")

#we save this data-frame, that we will use to perform analyses in SPSS
# Define the file path
file_path_2 <- here("Reports", "Report_output", "Dataframes", "Whole_sample", "For_ANOVA_intensity_whole.csv")

# Save the dataframe to the specified path
write.csv(ave_intens_per_subject, file_path_2, row.names = FALSE)

rm(EE_file, group1, group2, file_path_2)
rm(ave_intens_per_subject)

###########################################################################
###########################################################################
###                                                                     ###
###                         EXTREMES DATAFRAMES                         ###
###                                                                     ###
###########################################################################
###########################################################################

#we remove pps not contained in the extreme groups 
participant_list <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066", "EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

df_emotion <- df_emotion %>%
  filter(Participant.Label %in% participant_list)

#we define the two lists of pp numbers based on extreme scores 
group1 <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066")
group2 <- c("EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

#we create a new column with the group label
df_emotion <- df_emotion %>%
  mutate(Group = case_when(
    Participant.Label %in% group1 ~ "Very Low EE",
    Participant.Label %in% group2 ~ "Very High EE",
    TRUE ~ NA_character_  
  ))

# Define the allowed values
allowed_values <- c("Very Low EE", "Very High EE")

# Check if there are any disallowed values
if (any(!df_emotion$Group %in% allowed_values)) {
  stop("Error: The 'Group' column contains values other than 'Very Low EE' and 'Very High EE'. The error occurred at line 213 in the script Report_script.R")
}

rm(allowed_values)

#we calculate in a new dataframe the cumulative frequency of specific emotions per food label per participant
sum_per_subject <- df_emotion %>%
  group_by(Participant.Label, Label) %>%
  summarise(Freq_Stressed = sum(Freq_Stressed),
            Freq_Sad = sum(`Freq_Sad/Depressed`),
            Freq_Happy = sum(Freq_Happy),
            Freq_Bored = sum(Freq_Bored))

#we add the column Group to our sum_per_subject dataframe
# Select the relevant columns to join
group_info <- df_emotion %>%
  select(Participant.Label, Group) %>%
  distinct()

# Join the group information with the summarized dataframe
sum_per_subject <- sum_per_subject %>%
  left_join(group_info, by = "Participant.Label")

rm(group_info)

#we add EE scores to the dataframe
#we read the original file with EE scores 
EE_file = read.csv(here("EE_calculation", "Dataframes", "Output_data", "EXTREME_R-processed_data_baseline.csv"), sep = ',')

#we only keep the columns of our interest
EE_file <- EE_file %>% select(EE_score, EoA)

#we remove pps that still have not finished EMA
pps_to_remove <- c("EoA116", "EoA117", "EoA118", "EoA119","EoA120", "EoA121", "EoA122", "EoA123", "EoA124", "EoA125", "EoA126", "EoA127", "EoA128", "EoA129", "Tessa_Test")
EE_file <- subset (EE_file, !(EoA %in% pps_to_remove))

#we bind the EE_scores to the sum_per_subject dataframe
EE_file <- EE_file %>%
  rename(Participant.Label = `EoA`)

sum_per_subject <- sum_per_subject %>%
  left_join(EE_file, by = "Participant.Label")

#we save this data-frame, that we will use to perform analyses in SPSS
# Define the file path
file_path_3 <- here("Reports","Report_output", "Dataframes", "Extremes_groups", "For_ANOVA_frequency_extremes.csv")

# Save the dataframe to the specified path
write.csv(sum_per_subject, file_path_3, row.names = FALSE)

rm(sum_per_subject, pps_to_remove, file_path_3)
########################################################################
#now we create a similar dataframe as before, but using the average instead of Intensities 
ave_intens_per_subject <- df_emotion %>%
  group_by(Participant.Label, Label) %>%
  summarise(Int_Stressed = mean(Intens_Stressed),
            Int_Sad = mean(`Intens_Sad/Depressed`),
            Int_Happy = mean(Intens_Happy),
            Int_Bored = mean(Intens_Bored))

#we add the column Group to our sum_per_subject dataframe
# Select the relevant columns to join
group_info <- df_emotion %>%
  select(Participant.Label, Group) %>%
  distinct()

# Join the group information with the summarized dataframe
ave_intens_per_subject <- ave_intens_per_subject %>%
  left_join(group_info, by = "Participant.Label")

rm(group_info)

#we add the EE scores
ave_intens_per_subject <- ave_intens_per_subject %>%
  left_join(EE_file, by = "Participant.Label")

#we save this data-frame, that we will use to perform analyses in SPSS
# Define the file path
file_path_4 <- here("Reports", "Report_output", "Dataframes", "Extremes_groups", "For_ANOVA_intensity_extremes.csv")

# Save the dataframe to the specified path
write.csv(ave_intens_per_subject, file_path_4, row.names = FALSE)
