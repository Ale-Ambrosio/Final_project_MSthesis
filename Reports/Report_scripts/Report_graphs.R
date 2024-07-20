#Graphs for the report
library(ggplot2)
library(reshape2)
#################################################################
##                  Frequency - Median Groups                  ##
#################################################################
#we load the data
Whole_frequency = read.csv2 (here ("Reports", "Report_output", "Dataframes", "Whole_sample", "For_ANOVA_frequency_whole.csv"),sep = ",")
names(Whole_frequency)[2] <- "FoodLabel"

# Melt the dataframe for easier plotting with ggplot2
df_melt <- melt(Whole_frequency, id.vars = c("Participant.Label", "FoodLabel", "Group"), 
                measure.vars = c("Freq_Stressed", "Freq_Sad", "Freq_Happy", "Freq_Bored"),
                variable.name = "Emotion", value.name = "Frequency")

#I'm renaming the emotions labels for the graph
df_melt <- df_melt %>%
  mutate(Emotion = case_when(
    Emotion == "Freq_Stressed" ~ "Stressed",
    Emotion == "Freq_Sad" ~ "Sad",
    Emotion == "Freq_Happy" ~ "Happy",
    Emotion == "Freq_Bored" ~ "Bored",
    TRUE ~ Emotion  # Keeps any other values unchanged
  ))

# Define the allowed values
allowed_values <- c("Stressed", "Sad", "Happy", "Bored")

# Check if there are any disallowed values
if (any(!df_melt$Emotion %in% allowed_values)) {
  stop("Error: The 'Emotion' column contains values other than Stressed, Sad, Happy, Bored. The error occurred at line 30 of the script Report_graphs.R")
}

rm(allowed_values)

# Calculate summary statistics
summary_df <- df_melt %>%
  group_by(FoodLabel, Group, Emotion) %>%
  summarize(mean_freq = mean(Frequency),
            sd_freq = sd(Frequency),
            .groups = 'drop')

# Define a function to create plots for each emotion
plot_emotion <- function(emotion) {
  ggplot() +
    # Add the points for individual data
    geom_jitter(data = df_melt %>% filter(Emotion == emotion), aes(x = FoodLabel, y = Frequency, color = Group), 
                width = 0.2, alpha = 0.5, size = 0.9) +
    # Add the error bars for standard deviation
    geom_errorbar(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, ymin = mean_freq - sd_freq, ymax = mean_freq + sd_freq, color = Group), 
                  width = 0.2, position = position_dodge(width = 0.3)) +
    # Add the points for means
    geom_point(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, y = mean_freq, color = Group, shape = Group), 
               size = 3, position = position_dodge(width = 0.3)) +
    # Customize plot
    labs(title = paste("Frequency of", emotion, "by Food Label and Group"),
         x = "",
         y = "Frequency") +
    theme_minimal() +
    scale_x_discrete(labels = unique(df_melt$FoodLabel))
}

# Generate plots for each emotion
plot_stressed <- plot_emotion("Stressed")
plot_sad <- plot_emotion("Sad")
plot_happy <- plot_emotion("Happy")
plot_bored <- plot_emotion("Bored")

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Frequency", "Stressed_f_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_stressed)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Frequency", "Sad_f_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_sad)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Frequency", "Happy_f_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_happy)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Frequency", "Bored_f_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_bored)
dev.off()

#################################################################
##                  Intensity - Median Groups                  ##
#################################################################

rm(list = ls())

#we load the data
Whole_intensity = read.csv2 (here ("Reports", "Report_output", "Dataframes", "Whole_sample", "For_ANOVA_intensity_whole.csv"),sep = ",")
names(Whole_intensity)[2] <- "FoodLabel"

# Melt the dataframe for easier plotting with ggplot2
library(ggplot2)
library(reshape2)
df_melt <- melt(Whole_intensity, id.vars = c("Participant.Label", "FoodLabel", "Group"), 
                measure.vars = c("Int_Stressed", "Int_Sad", "Int_Happy", "Int_Bored"),
                variable.name = "Emotion", value.name = "Intensity")

#I'm renaming the emotions labels for the graph
df_melt <- df_melt %>%
  mutate(Emotion = case_when(
    Emotion == "Int_Stressed" ~ "Stressed",
    Emotion == "Int_Sad" ~ "Sad",
    Emotion == "Int_Happy" ~ "Happy",
    Emotion == "Int_Bored" ~ "Bored",
    TRUE ~ Emotion  # Keeps any other values unchanged
  ))

# Define the allowed values
allowed_values <- c("Stressed", "Sad", "Happy", "Bored")

# Check if there are any disallowed values
if (any(!df_melt$Emotion %in% allowed_values)) {
  stop("Error: The 'Emotion' column contains values other than Stressed, Sad, Happy, Bored. The error occurred at line 116 of the script Report_graphs.R")
}

rm(allowed_values)

#converting intensity to numeric
df_melt$Intensity <- as.numeric(df_melt$Intensity)

# Calculate summary statistics
summary_df <- df_melt %>%
  group_by(FoodLabel, Group, Emotion) %>%
  summarize(mean_int = mean(Intensity),
            sd_int = sd(Intensity),
            .groups = 'drop')

# Define a function to create plots for each emotion
plot_emotion <- function(emotion) {
  ggplot() +
    # Add the points for individual data
    geom_jitter(data = df_melt %>% filter(Emotion == emotion), aes(x = FoodLabel, y = Intensity, color = Group), 
                width = 0.2, alpha = 0.5, size = 0.9) +
    # Add the error bars for standard deviation
    geom_errorbar(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, ymin = mean_int - sd_int, ymax = mean_int + sd_int, color = Group), 
                  width = 0.2, position = position_dodge(width = 0.3)) +
    # Add the points for means
    geom_point(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, y = mean_int, color = Group, shape = Group), 
               size = 3, position = position_dodge(width = 0.3)) +
    # Customize plot
    labs(title = paste("Intensity of", emotion, "by Food Label and Group"),
         x = "",
         y = "Intensity") +
    theme_minimal() +
    scale_x_discrete(labels = unique(df_melt$FoodLabel))
}

# Generate plots for each emotion
plot_stressed <- plot_emotion("Stressed")
plot_sad <- plot_emotion("Sad")
plot_happy <- plot_emotion("Happy")
plot_bored <- plot_emotion("Bored")

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Intensity", "Stressed_i_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_stressed)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Intensity", "Sad_i_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_sad)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Intensity", "Happy_i_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_happy)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Whole_sample", "Intensity", "Bored_i_w.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_bored)
dev.off()

##################################################################
##                  Frequency - Extreme Groups                  ##
##################################################################
rm(list = ls())

#we load the data
Whole_frequency = read.csv2 (here ("Reports", "Report_output", "Dataframes", "Extremes_groups", "For_ANOVA_frequency_extremes.csv"),sep = ",")
names(Whole_frequency)[2] <- "FoodLabel"

# Melt the dataframe for easier plotting with ggplot2
df_melt <- melt(Whole_frequency, id.vars = c("Participant.Label", "FoodLabel", "Group"), 
                measure.vars = c("Freq_Stressed", "Freq_Sad", "Freq_Happy", "Freq_Bored"),
                variable.name = "Emotion", value.name = "Frequency")

#I'm renaming the emotions labels for the graph
df_melt <- df_melt %>%
  mutate(Emotion = case_when(
    Emotion == "Freq_Stressed" ~ "Stressed",
    Emotion == "Freq_Sad" ~ "Sad",
    Emotion == "Freq_Happy" ~ "Happy",
    Emotion == "Freq_Bored" ~ "Bored",
    TRUE ~ Emotion  # Keeps any other values unchanged
  ))

# Define the allowed values
allowed_values <- c("Stressed", "Sad", "Happy", "Bored")

# Check if there are any disallowed values
if (any(!df_melt$Emotion %in% allowed_values)) {
  stop("Error: The 'Emotion' column contains values other than Stressed, Sad, Happy, Bored. The error occurred at line 207 of the script Report_graphs.R")
}

rm(allowed_values)

# Calculate summary statistics
summary_df <- df_melt %>%
  group_by(FoodLabel, Group, Emotion) %>%
  summarize(mean_freq = mean(Frequency),
            sd_freq = sd(Frequency),
            .groups = 'drop')

# Define a function to create plots for each emotion
plot_emotion <- function(emotion) {
  ggplot() +
    # Add the points for individual data
    geom_jitter(data = df_melt %>% filter(Emotion == emotion), aes(x = FoodLabel, y = Frequency, color = Group), 
                width = 0.2, alpha = 0.5, size = 0.9) +
    # Add the error bars for standard deviation
    geom_errorbar(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, ymin = mean_freq - sd_freq, ymax = mean_freq + sd_freq, color = Group), 
                  width = 0.2, position = position_dodge(width = 0.3)) +
    # Add the points for means
    geom_point(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, y = mean_freq, color = Group, shape = Group), 
               size = 3, position = position_dodge(width = 0.3)) +
    # Customize plot
    labs(title = paste("Frequency of", emotion, "by Food Label and Group"),
         x = "",
         y = "Frequency") +
    theme_minimal() +
    scale_x_discrete(labels = unique(df_melt$FoodLabel))
}

# Generate plots for each emotion
plot_stressed <- plot_emotion("Stressed")
plot_sad <- plot_emotion("Sad")
plot_happy <- plot_emotion("Happy")
plot_bored <- plot_emotion("Bored")

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Frequency", "Stressed_f_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_stressed)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Frequency", "Sad_f_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_sad)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Frequency", "Happy_f_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_happy)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Frequency", "Bored_f_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_bored)
dev.off()

##################################################################
##                  Intensity - Extreme Groups                  ##
##################################################################

rm(list = ls())

#we load the data
Whole_intensity = read.csv2 (here ("Reports", "Report_output", "Dataframes", "Extremes_groups", "For_ANOVA_intensity_extremes.csv"),sep = ",")
names(Whole_intensity)[2] <- "FoodLabel"

# Melt the dataframe for easier plotting with ggplot2
library(ggplot2)
library(reshape2)
df_melt <- melt(Whole_intensity, id.vars = c("Participant.Label", "FoodLabel", "Group"), 
                measure.vars = c("Int_Stressed", "Int_Sad", "Int_Happy", "Int_Bored"),
                variable.name = "Emotion", value.name = "Intensity")

#I'm renaming the emotions labels for the graph
df_melt <- df_melt %>%
  mutate(Emotion = case_when(
    Emotion == "Int_Stressed" ~ "Stressed",
    Emotion == "Int_Sad" ~ "Sad",
    Emotion == "Int_Happy" ~ "Happy",
    Emotion == "Int_Bored" ~ "Bored",
    TRUE ~ Emotion  # Keeps any other values unchanged
  ))

# Define the allowed values
allowed_values <- c("Stressed", "Sad", "Happy", "Bored")

# Check if there are any disallowed values
if (any(!df_melt$Emotion %in% allowed_values)) {
  stop("Error: The 'Emotion' column contains values other than Stressed, Sad, Happy, Bored. The error occurred at line 293 of the script Report_graphs.R")
}

rm(allowed_values)

#converting intensity to numeric
df_melt$Intensity <- as.numeric(df_melt$Intensity)

# Calculate summary statistics
summary_df <- df_melt %>%
  group_by(FoodLabel, Group, Emotion) %>%
  summarize(mean_int = mean(Intensity),
            sd_int = sd(Intensity),
            .groups = 'drop')

# Define a function to create plots for each emotion
plot_emotion <- function(emotion) {
  ggplot() +
    # Add the points for individual data
    geom_jitter(data = df_melt %>% filter(Emotion == emotion), aes(x = FoodLabel, y = Intensity, color = Group), 
                width = 0.2, alpha = 0.5, size = 0.9) +
    # Add the error bars for standard deviation
    geom_errorbar(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, ymin = mean_int - sd_int, ymax = mean_int + sd_int, color = Group), 
                  width = 0.2, position = position_dodge(width = 0.3)) +
    # Add the points for means
    geom_point(data = summary_df %>% filter(Emotion == emotion), aes(x = FoodLabel, y = mean_int, color = Group, shape = Group), 
               size = 3, position = position_dodge(width = 0.3)) +
    # Customize plot
    labs(title = paste("Intensity of", emotion, "by Food Label and Group"),
         x = "",
         y = "Intensity") +
    theme_minimal() +
    scale_x_discrete(labels = unique(df_melt$FoodLabel))
}

# Generate plots for each emotion
plot_stressed <- plot_emotion("Stressed")
plot_sad <- plot_emotion("Sad")
plot_happy <- plot_emotion("Happy")
plot_bored <- plot_emotion("Bored")

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Intensity", "Stressed_i_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_stressed)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Intensity", "Sad_i_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_sad)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Intensity", "Happy_i_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_happy)
dev.off()

jpeg(here("Reports", "Report_output", "Graphs", "Extremes_groups", "Intensity", "Bored_i_e.jpg"), width = 10, height = 6, units = "in", res = 300)
print(plot_bored)
dev.off()
