library(here) # initialize the root path
library (dplyr)
library(officer)
library(ggplot2)

# Paths for the raw data files
path_EE_raw <- here("EE_calculation", "Dataframes", "Input_data", "Baseline_measures.csv")
path_Qualtrics_ID <- here ("EE_calculation", "Dataframes", "Input_data", "Qualtrics ID.csv")

# Read the data from CSV file
data_baseline <- read.csv2(path_EE_raw, sep = ",")
ID_list <- read.csv2 (path_Qualtrics_ID, sep = ",")

rm(path_EE_raw, path_Qualtrics_ID)

#Get rid of useless rows and columns 
data_baseline <- data_baseline[-c(1, 2), ]

# Columns to keep
columns_to_keep <- c("RecipientLastName")

# Identify columns that start with "Vragenlijst..7_"
vragenlijst_columns <- grep("^Vragenlijst\\.\\.7_", names(data_baseline), value = TRUE)

# Combine with columns_to_keep
columns_to_keep <- c(columns_to_keep, vragenlijst_columns)

# Subset the dataframe
data_baseline <- data_baseline[, columns_to_keep]

rm(columns_to_keep)

#checking for duplicates
# Remove duplicated rows
data_baseline <- unique(data_baseline)

#checking for NAs
#replacing empty cells with NA
data_baseline = replace(data_baseline, data_baseline == "", NA)

#remove rows with all NAs

# Identify rows where all values in EE are NA
na_rows <- data_baseline %>%
  filter(rowSums(is.na(select(., vragenlijst_columns))) == 33) 

# Count the total number of such rows
total_na_rows <- nrow(na_rows)

# Print the results
print(paste("Total number of rows with all NA:", total_na_rows))

# Remove these rows from the original data frame
data_baseline <- data_baseline %>%
  filter(!(rowSums(is.na(select(., vragenlijst_columns))) == 33))

rm(na_rows)

##here we add the Eoa numbers to our Qualtrics data 

#removing duplicated rows in the Qualtrics ID file

ID_list = ID_list[!duplicated(ID_list),]

#we remove all the participants that are not done yet with the EMA 
pps_to_remove <- c("EoA116", "EoA117", "EoA118", "EoA119","EoA120", "EoA121", "EoA122", "EoA123", "EoA124", "EoA125", "EoA126", "EoA127", "EoA128", "EoA129", "Tessa_Test")
ID_list <- subset (ID_list, !(EoA %in% pps_to_remove))

#binding the EoA codes in the baseline dataframe
# Rename the columns in ID_list to make the join easier 
ID_list <- ID_list %>%
  rename(RecipientLastName = `Qualtrics.code`)

# Perform the left join to add the EoA column to data_baseline
data_baseline <- data_baseline %>%
  left_join(ID_list, by = "RecipientLastName")

#we need to remove pps that don't have an EoA code (drop-out)
data_baseline <- data_baseline[complete.cases(data_baseline$EoA), ]

#we remove the duplicate for EoA090 (1156860e5fedbd6157c7)
data_baseline <- data_baseline[data_baseline$RecipientLastName != "1156860e5fedbd6157c7", ]

rm(pps_to_remove, total_na_rows)

# Define the positions of the columns you want to select within the vragenlijst_columns vector
selected_positions <- c(1, 3, 5, 8, 10, 13, 16, 20, 23, 25, 28, 30, 32)

# Select the column names corresponding to these positions
selected_column_names <- vragenlijst_columns[selected_positions]

# Get the actual column numbers in the data_baseline dataset
selected_column_numbers <- match(selected_column_names, names(data_baseline))

#we calculate for each subject the sum score of EE items

# Specify the columns you want to sum (EE items)
columns_to_sum = selected_column_numbers

# Ensure the specified columns are numeric
data_baseline[, columns_to_sum] <- lapply(data_baseline[, columns_to_sum], as.numeric)

# Calculate the sum of these columns for each row and store in a new column
data_baseline <- data_baseline %>%
  mutate(EE_score = rowSums(select(., all_of(columns_to_sum)), na.rm = TRUE))

rm(selected_column_names, selected_column_numbers, selected_positions)

#we assess the distribution of EE scores
#first we check the quartiles 
summary = summary(data_baseline$EE_score)
median = summary["Median"]

rm(summary)

#then we check the extreme scores (20th and 80th percentile)
percentiles <- quantile(data_baseline$EE_score, probs = c(0.20, 0.80))

lower_lim = percentiles ["20%"]
upper_lim = percentiles ["80%"]

rm(percentiles)

#then we build some graphs

column_name = "EE_score"

jpeg(here("EE_calculation", "Graphs","Density_plot_EE_score.jpg"), width=1200,height=600, res=300, quality=100)  # Opens a new JPEG device

# Create a histogram with density plot
ggplot(data_baseline, aes_string(x = column_name)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Probability Distribution of EE score",
    x = column_name,
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 8),     # Change the size of the axis titles
    axis.text = element_text(size = 6)       # Change the size of the axis labels
  )

dev.off()

jpeg(here("EE_calculation", "Graphs","Box_plot_EE_score.jpg"), width=1200,height=1000, res=300, quality=100)  # Opens a new JPEG device

# Create a box plot
ggplot(data_baseline, aes_string(y = column_name, x = 1)) +  # Add x = 1 to place all data in one box
  geom_boxplot(fill = "lightblue", color = "darkblue", width = 0.2) +  # Adjust width to make box narrower
  labs(
    title = "Box Plot of EE score",
    x = "",  # Remove x-axis label since it's not needed
    y = column_name
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),    # Change the size of the axis titles
    axis.text = element_text(size = 10)    # Change the size of the axis labels
  )

dev.off()

#then we select participants on the basis of two different cut-offs

###1 is the one based on the median
#we create a copy of the database
data_base_median = data_baseline 

# Create the new column based on the condition
data_base_median <- data_base_median %>%
  mutate(EE_category_Med = if_else(EE_score < median, 1, 2))

#Reorder the dataset based on the EE_category
data_base_median <- data_base_median %>%
  arrange(EE_category_Med)

#Reorder the dataset based on the EE score
data_base_median <- data_base_median %>%
  arrange(EE_score)

#obtain the list of EoA numbers for the two groups 
# Extract EoA values where EE_category_Med is 0
eoa_0 <- data_base_median %>% filter(EE_category_Med == 1) %>% pull(EoA)

# Extract EoA values where EE_category_Med is 1
eoa_1 <- data_base_median %>% filter(EE_category_Med == 2) %>% pull(EoA)

# Manually add quotation marks around each value and create comma-separated strings
eoa_0_str <- paste0('"', eoa_0, '"', collapse = ", ")
eoa_1_str <- paste0('"', eoa_1, '"', collapse = ", ")

# Print the comma-separated lists with quotation marks
cat("Low emotional eaters:\n", eoa_0_str, "\n")
cat("High emotional eaters:\n", eoa_1_str, "\n")

#we can save this list into a doc file
# Create a new Word document
doc <- read_docx()

# Add the comma-separated lists with quotation marks to the document
doc <- doc %>%
  body_add_par("Low emotional eaters:", style = "Normal") %>%
  body_add_par(eoa_0_str, style = "Normal") %>%
  body_add_par("High emotional eaters:", style = "Normal") %>%
  body_add_par(eoa_1_str, style = "Normal")

# Specify the path where the file will be saved
file_path_1 <- here("EE_calculation", "Dataframes", "Output_data", "EoA_codes_median.docx")

# Save the document
print(doc, target = file_path_1)

# Define the file path
file_path_2 <- here("EE_calculation", "Dataframes", "Output_data","MEDIAN_R-processed_data_baseline.csv")

# Save the reordered dataset to the specified path
write.csv(data_base_median, file_path_2, row.names = FALSE)

rm(file_path_1, file_path_2, eoa_0, eoa_0_str, eoa_1, eoa_1_str)
rm(doc, data_base_median)

###2 we select only the pps with extreme scores 
#we make a copy of the database
data_base_extreme = data_baseline

# Create the new column based on the condition
data_base_extreme <- data_base_extreme %>%
  mutate(EE_category_Ext = if_else(EE_score < lower_lim, 1, ifelse(EE_score > upper_lim, 2, 0)))

#Reorder the dataset based on the EE_category
data_base_extreme <- data_base_extreme %>%
  arrange(EE_category_Ext)

#We remove the pps between the 20th and 80th percentile 
data_base_extreme <- data_base_extreme %>% filter(EE_category_Ext != 0)

#Reorder the dataset based on the EE score
data_base_extreme <- data_base_extreme %>%
  arrange(EE_score)

#obtain the list of EoA numbers for the two groups 
# Extract EoA values where EE_category_Ext is 1
eoa_e_1 <- data_base_extreme %>% filter(EE_category_Ext == 1) %>% pull(EoA)

# Extract EoA values where EE_category_Ext is 2
eoa_e_2 <- data_base_extreme %>% filter(EE_category_Ext == 2) %>% pull(EoA)

# Manually add quotation marks around each value and create comma-separated strings
eoa_e_1_str <- paste0('"', eoa_e_1, '"', collapse = ", ")
eoa_e_2_str <- paste0('"', eoa_e_2, '"', collapse = ", ")

# Print the comma-separated lists with quotation marks
cat("Very low emotional eaters:\n", eoa_e_1_str, "\n")
cat("Very high emotional eaters:\n", eoa_e_2_str, "\n")

#we can save this list into a doc file
# Create a new Word document
doc2 <- read_docx()

# Add the comma-separated lists with quotation marks to the document
doc2 <- doc2 %>%
  body_add_par(" Very low emotional eaters:", style = "Normal") %>%
  body_add_par(eoa_e_1_str, style = "Normal") %>%
  body_add_par("Very high emotional eaters:", style = "Normal") %>%
  body_add_par(eoa_e_2_str, style = "Normal")

# Specify the path where the file will be saved
file_path_b <- here("EE_calculation", "Dataframes", "Output_data", "EoA_codes_extreme.docx")

# Save the document
print(doc2, target = file_path_b)

# Define the file path
file_path2 <- here("EE_calculation", "Dataframes", "Output_data", "EXTREME_R-processed_data_baseline.csv")

# Save the reordered dataset to the specified path
write.csv(data_base_extreme, file_path2, row.names = FALSE)
