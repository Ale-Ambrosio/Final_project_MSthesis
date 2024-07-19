#Report script
#Adapting the report script 

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

source(here("Report_scripts","Main_child.R"))

rm(participants_with_few_observations, selected_columns, unique_values)

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
  'Freq_Worried/Anxious' = 'Categ_EMOTIE_Bang.angstig',
  'Freq_Irritated/Angry' = 'Categ_EMOTIE_Geïrriteerd.boos',
  'Freq_Stressed' = 'Categ_EMOTIE_Gestrest',
  'Freq_Relaxed/Calm' = 'Categ_EMOTIE_Ontspannen.kalm',
  'Freq_Happy' = 'Categ_EMOTIE_Blij.vrolijk',
  'Freq_Sad/Depressed' = 'Categ_EMOTIE_Droevig.somber',
  'Freq_Bored' = 'Categ_EMOTIE_Verveeld',
  'Freq_Tired' = 'Categ_EMOTIE_Vermoeid',
  'Freq_Energetic' = 'Categ_EMOTIE_Energiek',
  'Freq_Self-confident' = 'Categ_EMOTIE_Zelfverzekerd',
  'Freq_Insecure' = 'Categ_EMOTIE_Onzeker',
  'Freq_Ashamed/Guilty' = 'Categ_EMOTIE_Beschaamd.schuldig',
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

#we finally remove from the database all the columns that we don't need 
# Columns to remove by their numbers
columns_to_remove <- (18:35)  

# Removing specified columns
df_emotion <- df_emotion %>% select(-columns_to_remove)
print(df_emotion)

###################################
#HERE we calculate the sum of food labels for each emotions' group
#we remove pps not contained in the extreme groups 
participant_list <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066", "EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

df_emotion <- df_emotion %>%
  filter(Participant.Label %in% participant_list)

#we do it separately for the two different groups of pps we define 

group1_labels <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066")
group2_labels <- c("EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

# Subset the dataframe for the two groups of participant labels
subset1_df <- df_emotion %>%
  filter(Participant.Label %in% group1_labels)

subset2_df <- df_emotion %>%
  filter(Participant.Label %in% group2_labels)

#checking if there are any unexptected values in the two subsets
subject_labels_group1 = unique (subset1_df$Participant.Label)
subject_labels_group2 = unique (subset2_df$Participant.Label)
#add a stop

# Print the subject labels
print(paste("Subject labels in group 1 are:", paste(subject_labels_group1, collapse = ", ")))
print(paste("Subject labels in group 2 are:", paste(subject_labels_group2, collapse = ", ")))


# Calculate the sum of each food label within each emotion category 
sum_g1 <- subset1_df %>%
  group_by(Label) %>%
  summarise(across(c(Positive_Emotion, Negative_Emotion, Neutral_Emotion), sum)) %>%
  pivot_longer(cols = c(Positive_Emotion, Negative_Emotion, Neutral_Emotion),
               names_to = "emotion",
               values_to = "value")

# Rename the "Label" column to "food label"
names(sum_g1)[1] <- "food label"


sum_g2 <- subset2_df %>%
  group_by(Label) %>%
  summarise(across(c(Positive_Emotion, Negative_Emotion, Neutral_Emotion), sum)) %>%
  pivot_longer(cols = c(Positive_Emotion, Negative_Emotion, Neutral_Emotion),
               names_to = "emotion",
               values_to = "value")

names(sum_g2)[1] <- "food label"

#We check if R respected the rationale of the calculation

# Counting rows on the basis of the label and the type of emotion present

check_label = "Gray Label"
check_emotion = "Positive_Emotion"

count <- subset2_df %>%
  filter(Label == check_label & get(check_emotion) > 0) %>%
  nrow()

cat("Number of rows where Label is", check_label, "and", check_emotion, "is greater than 0:", count, "\n")

#HERE we do the same but using percentages instead of sums 

# Calculate the percentage of each food label within each emotion category (positive and negative)
percentage_g1 <- subset1_df %>%
  group_by(Label) %>%
  summarise(across(c(Positive_Emotion, Negative_Emotion, Neutral_Emotion), ~ sum(.))) %>%
  mutate(across(c(Positive_Emotion, Negative_Emotion, Neutral_Emotion), ~ ./sum(.)*100))%>%
  pivot_longer(cols = c(Positive_Emotion, Negative_Emotion, Neutral_Emotion),
               names_to = "emotion",
               values_to = "value")

percentage_g2 <- subset2_df %>%
  group_by(Label) %>%
  summarise(across(c(Positive_Emotion, Negative_Emotion, Neutral_Emotion), ~ sum(.))) %>%
  mutate(across(c(Positive_Emotion, Negative_Emotion, Neutral_Emotion), ~ ./sum(.)*100))%>%
  pivot_longer(cols = c(Positive_Emotion, Negative_Emotion, Neutral_Emotion),
               names_to = "emotion",
               values_to = "value")

####################################
#Stats and graphs 

#The previous processing was needed to create the graphs, which we will try to process below here
library(gridExtra)
library(patchwork)

#Graphs for the sum
plot_emotions_sum1 = ggplot(sum_g1, aes(x = emotion, y = value, fill = `food label`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors) +
  #facet_wrap(~`food label`, dir = "v") +
  labs(x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = 15),  # Increase x-axis label font size
        axis.text.y = element_text(size = 15),  # Increase y-axis label font size
        axis.title = element_text(size = 15, face='bold'),  # Increase axis title font size
        plot.title = element_text(size = 15, face ='bold'),  # Increase plot title font size
        legend.position = "none")+
  ggtitle("Low EE")

plot_emotions_sum2 = ggplot(sum_g2, aes(x = emotion, y = value, fill = `food label`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors) +
  #facet_wrap(~`food label`, dir = "v") +
  labs(x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, size = 15),  # Increase x-axis label font size
        axis.text.y = element_text(size = 15),  # Increase y-axis label font size
        axis.title = element_text(size = 15, face='bold'),  # Increase axis title font size
        plot.title = element_text(size = 15, face ='bold'),  # Increase plot title font size
        legend.position = "none")+
  ggtitle("High EE")

# Combine the two plots side by side
jpeg("report_barplot_extreme.jpg", width=3600,height=1800, res=300, quality=100)  # Opens a new JPEG device
combined_plot <- plot_emotions_sum1 + plot_emotions_sum2 + plot_layout(ncol = 2)

# Print the combined plot
print(combined_plot)

dev.off()

#here we create a single graph for both groups
#first we combine the dataset
sum_g1 <- sum_g1 %>% mutate(group = "Group 1")
sum_g2 <- sum_g2 %>% mutate(group = "Group 2")

combined_sum <- bind_rows(sum_g1, sum_g2)

#now we plot it
jpeg("report_barplot_whole_sample_2.jpg", width=3600,height=1800, res=300, quality=100)  # Opens a new JPEG device

plot_emotions_combined <- ggplot(combined_sum, aes(x = emotion, y = value, fill = `food label`)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ group, scales = "free_y") +
  coord_flip()+
  scale_fill_manual(values = colors) +
  labs(x = "Emotion", y = "Value", fill = "Food Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 15),  
        axis.text.y = element_text(size = 15),  
        axis.title = element_text(size = 15, face = 'bold'),  
        plot.title = element_text(size = 15, face = 'bold'))

# Print the combined plot
print(plot_emotions_combined)

dev.off()

##second version
jpeg("report_barplot_whole_sample_2.jpg", width=3600,height=1800, res=300, quality=100)  # Opens a new JPEG device

plot_emotions_combined <- ggplot(combined_sum, aes(x = emotion, y = value, fill = `food label`, group = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  coord_flip() +
  scale_fill_manual(values = colors) +
  labs(x = "Emotion", y = "Value", fill = "Food Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 15),  
        axis.text.y = element_text(size = 15),  
        axis.title = element_text(size = 15, face = 'bold'),  
        plot.title = element_text(size = 15, face = 'bold'))

# Print the combined plot
print(plot_emotions_combined)

dev.off()

#could not manage to figure out how to put a legend for the two groups :(


#############
#we define the two lists of pp numbers based on the median
group1 <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066", "EoA071", "EoA081", "EoA097", "EoA110", "EoA037", "EoA057", "EoA056", "EoA070", "EoA033", "EoA061", "EoA073", "EoA080", "EoA010", "EoA087", "EoA011", "EoA014", "EoA032", "EoA039", "EoA086", "EoA004", "EoA036", "EoA055", "EoA096", "EoA018", "EoA048", "EoA103", "EoA007", "EoA064", "EoA105", "EoA106", "EoA109")
group2 <- c("EoA030", "EoA044", "EoA045", "EoA077", "EoA102", "EoA022", "EoA023", "EoA052", "EoA091", "EoA002", "EoA012", "EoA015", "EoA042", "EoA078", "EoA083", "EoA092", "EoA104", "EoA008", "EoA031", "EoA035", "EoA038", "EoA069", "EoA043", "EoA067", "EoA075", "EoA024", "EoA063", "EoA089", "EoA100", "EoA090", "EoA114", "EoA034", "EoA108", "EoA006", "EoA085", "EoA115", "EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

#we define the two lists of pp numbers based on extreme scores 
#group1 <- c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066")
#group2 <- c("EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019")

#we create a new column with the group label
df_emotion <- df_emotion %>%
  mutate(Group = case_when(
    Participant.Label %in% group1 ~ "Very Low EE",
    Participant.Label %in% group2 ~ "Very High EE",
    TRUE ~ NA_character_  # Optional: Handle participants not in either group
  ))

#we calculate in a new dataframe the cumulative frequency of specific emotions per food label per participant
sum_per_subject <- df_emotion %>%
  group_by(Participant.Label, Label) %>%
  summarise(Freq_Stressed = sum(Freq_Stressed),
            Freq_Sad = sum(`Freq_Sad/Depressed`),
            Freq_Happy = sum(Freq_Happy),
            Freq_Bored = sum(Freq_Bored))

#to check if it worked properly
filtered_data <- df_emotion %>%
  filter(Participant.Label == "EoA003" & Label == "Red Label") %>%
  select(`Freq_Sad/Depressed`, Freq_Happy)

rm(filtered_data)

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
EE_file = read.csv("C:/Users/ambro/Downloads/Network coding/Scripts_EoA/EE_data/EXTREME_R-processed_data_baseline.csv", sep = ',')

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

#we add a new column for regression
sum_per_subject <- sum_per_subject %>%
  mutate(Group_Code = case_when(
    Group == "Very Low EE" ~ 0,
    Group == "Very High EE" ~ 1,
    TRUE ~ NA_real_  # This line ensures that any other values in Group get NA
  ))

sum_per_subject <- sum_per_subject %>%
  mutate(Food_Code = case_when(
    Label == "Gray Label" ~ 0,
    Label == "Red Label" ~ 1,
    Label == "Green Label" ~ 2,
    TRUE ~ NA_real_  # This line ensures that any other values in Group get NA
  ))

#we save this data-frame, that we will use to perform analyses in SPSS
# Define the file path
file_path_AN <- "C:/Users/ambro/Downloads/Network coding/Scripts_EoA/Report_data/Dataframe_for_ANOVA_extreme_frequency.csv"

# Save the dataframe to the specified path
write.csv(sum_per_subject, file_path_AN, row.names = FALSE)

########################################################################
#after having created the dataframe we needed 
#we can perform the ANOVA

# Function to perform ANOVA and extract p-values and means
perform_anova <- function(response_var, data) {
  # Perform ANOVA
  formula <- as.formula(paste(response_var, "~ Label * Group"))
  aov_result <- aov(formula, data = data)
  
  # Extract p-values
  aov_summary <- summary(aov_result)[[1]]
  p_values <- as.data.frame(aov_summary)["Pr(>F)"]
  p_values <- data.frame(Term = rownames(p_values), P_Value = p_values$`Pr(>F)`)
  
  # Calculate means for each combination of Label and Group
  means <- data %>%
    group_by(Label, Group) %>%
    summarise(Mean = mean(.data[[response_var]], na.rm = TRUE)) %>%
    ungroup()
  
  # Combine p-values and means
  result <- list(p_values = p_values, means = means)
  return(result)
}

# List of response variables
response_vars <- c("Freq_Stressed", "Freq_Sad", "Freq_Happy", "Freq_Bored")

# Perform ANOVAs and store results
anova_results <- lapply(response_vars, function(var) perform_anova(var, sum_per_subject))
names(anova_results) <- response_vars

# Create dataframes for each ANOVA result
for (var in response_vars) {
  assign(paste0("anova_", var, "_pvalues"), anova_results[[var]]$p_values)
  assign(paste0("anova_", var, "_means"), anova_results[[var]]$means)
}

# Function to plot means for a given response variable
plot_means <- function(means_df, response_var) {
  ggplot(means_df, aes(x = Label, y = Mean, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste("Mean", response_var, "by Label and Group"), 
         x = "Label", y = paste("Mean", response_var)) +
    theme_minimal()
}

# Create and display plots for each response variable
for (var in response_vars) {
  means_df <- get(paste0("anova_", var, "_means"))
  plot <- plot_means(means_df, var)
  print(plot)
}
########################################################################
#now we create a similar dataframe as before, but using the average instead of Intensities 
ave_intens_per_subject <- df_emotion %>%
  group_by(Participant.Label, Label) %>%
  summarise(Int_Stressed = mean(Intens_Stressed),
            Int_Sad = mean(`Intens_Sad/Depressed`),
            Int_Happy = mean(Intens_Happy),
            Int_Bored = mean(Intens_Bored))

#to check if it worked properly
filtered_data <- df_emotion %>%
  filter(Participant.Label == "EoA009" & Label == "Gray Label") %>%
  select(Intens_Happy, Intens_Bored)

rm(filtered_data)

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

#we add new columns for moderated regression
ave_intens_per_subject <- ave_intens_per_subject %>%
  mutate(Group_Code = case_when(
    Group == "Very Low EE" ~ 0,
    Group == "Very High EE" ~ 1,
    TRUE ~ NA_real_  # This line ensures that any other values in Group get NA
  ))

ave_intens_per_subject <- ave_intens_per_subject %>%
  mutate(Food_Code = case_when(
    Label == "Gray Label" ~ 0,
    Label == "Red Label" ~ 1,
    Label == "Green Label" ~ 2,
    TRUE ~ NA_real_  # This line ensures that any other values in Group get NA
  ))

#we save this data-frame, that we will use to perform analyses in SPSS
# Define the file path
file_path_AN_2 <- "C:/Users/ambro/Downloads/Network coding/Scripts_EoA/Report_data/Dataframe_for_ANOVA_extreme_intensity.csv"

# Save the dataframe to the specified path
write.csv(ave_intens_per_subject, file_path_AN_2, row.names = FALSE)

rm(EE_file)
