## First check if all packages are installed, otherwise install

list.of.packages <- c("devtools", "ggplot2", "tidyverse", "dplyr","xtable","stargazer","here",
                      "lubridate","gridExtra","skimr","tidyr","shiny","report","forcats",
                      "stringr", "purrr", "readr", "tibble", "shiny", 'plotly','mlVAR', 'Matrix')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Now code starts

library(plotly)
library(shiny)
library(dplyr)
library(lubridate)
library("mlVAR")
library("graphicalVAR")
library("qgraph")
library(gridExtra)
library (Matrix)
library(here) # initialize the root path

###########################################################################
###########################################################################
###                                                                     ###
###                         CHUNK CONCATENATION                         ###
###                                                                     ###
###########################################################################
###########################################################################


#Before reading the raw data we need to concatenate the chunks of data downloaded from Avicenna
#Reading the files 
check_0_25 = read.csv2(here("Networks","Network_data","Input_data","All_Participants",'lifestyle_all_0-25.csv'), sep = ',')
check_26_50 = read.csv2(here("Networks","Network_data","Input_data","All_Participants",'lifestyle_all_26-50.csv'), sep = ',')
check_50_100 = read.csv2(here("Networks","Network_data","Input_data","All_Participants",'lifestyle_all_50-100.csv'), sep = ',')
check_101_129 = read.csv2(here("Networks","Network_data","Input_data","All_Participants",'lifestyle_all_101-129.csv'), sep = ',')

#we remove the wrong labels 
pps_to_remove_1 <- c("EoA026")

if (any(check_0_25$Participant.Label %in% pps_to_remove_1)) {
  check_0_25 <- subset(check_0_25, !(Participant.Label %in% pps_to_remove_1))
}

#we remove from the last chunk all the participants that are not done yet with the EMA 
pps_to_remove_2 <- c("EoA116", "EoA117", "EoA118", "EoA119","EoA120", "EoA121", "EoA122", "EoA123", "EoA124", "EoA125", "EoA126", "EoA127", "EoA128", "EoA129", "Tessa_Test")
check_101_129 <- subset (check_101_129, !(Participant.Label %in% pps_to_remove_2))

# Check dimensions of data frames
if (!all(ncol(check_0_25) == ncol(check_26_50) & ncol(check_26_50) == ncol(check_50_100) & ncol(check_50_100) == ncol(check_101_129))) {
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that they have the same number of columns failed. This happened at line 48 in the EM_n.R script.")
}

# Check column names of data frames
if (!all(names(check_0_25) == names(check_26_50) &
         names(check_26_50) == names(check_50_100) &
         names(check_50_100) == names(check_101_129))) {
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that all columns have the same name has failed. This happened at line 55 in the EM_n.R script.")
}

concatenated_df = bind_rows (check_0_25, check_26_50, check_50_100, check_101_129)
data_path <- file.path (here("Networks","Network_data","Input_data", "All_Participants", "lifestyle_all.csv"))
write.csv(concatenated_df,data_path, row.names = FALSE)

rm(check_0_25, check_26_50, check_50_100, check_101_129, concatenated_df)

####### We read the EM_ df

signal_df = read.csv2(here("Networks","Network_data","Input_data", "All_Participants", "lifestyle_all.csv"), sep = ',')

###########################################################################
###########################################################################
###                                                                     ###
###                        DATAFRAME ADJUSTMENTS                        ###
###                                                                     ###
###########################################################################
###########################################################################

########### Get rid of metadata -----------------------------------------------------

# Identify columns containing 'metadata' in their names
metadata_cols <- grep("metadata", names(signal_df), ignore.case = TRUE)


# Remove columns containing 'metadata' 
signal_df <- signal_df[, -metadata_cols]


# remove metadata variable
rm(metadata_cols)

# renaming columns

old_to_new_columns = c(
  'Participant.ID' = 'Participant.ID',
  'Participant.Label' = 'Participant.Label',
  'Session.Scheduled.Time' = 'Session.Scheduled.Time',
  
  
  'Anxious' = 'X.1_VAS..Bang',
  'Irritated' = 'X.2_VAS..Geïrriteerd',
  'Stressed' = 'X.3_VAS..Stress',
  'Happy' = 'X.4_VAS..Blij',
  'Sad' = 'X.5_VAS..Droevig',
  'Bored' = 'X.6_VAS..Verveeld',
  'Energetic' = 'X.7_VAS..Energiek',
  'Self-confident' = 'X.8_VAS..Zelfverzekerd',
  'Ashamed' = 'X.9_VAS..Schaamte',
  'Lonely' = 'X.10_VAS..Eenzaam',
  'Craving' = "X.16_VAS..Trek")

#c("Anxious", "Irritated", "Stressed", "Happy", "Sad", "Bored", "Energetic", "Self-confident", "Ashamed", "Lonely", "Craving")

# now renaming

signal_df = signal_df %>% rename(old_to_new_columns)

# Now cutting signal_df to only the renamed columns above

# Extract the renamed column names
renamed_columns <- names(old_to_new_columns)

# Subset the EM_ data frame to keep only the renamed columns
signal_df <- signal_df %>% select(any_of((renamed_columns)))

rm(renamed_columns, old_to_new_columns)

############################################################################
############################################################################
###                                                                      ###
###                MISSINGNESS ANALYSIS AND REMOVAL OF NA                ###
###                                                                      ###
############################################################################
############################################################################

# List of columns to check for unique values
cols <- c("Anxious", "Irritated", "Stressed", "Happy", "Sad", "Bored", "Energetic", "Self-confident", "Ashamed", "Lonely", "Craving")

# Remove these rows from the original data frame
signal_df <- signal_df %>%
  filter(!(rowSums(is.na(select(., 4:14))) == 11))

# Create a flag column to mark rows where 'Trek' is NA 
signal_df <- signal_df %>%
  ungroup() %>%  # Ungroup the data to avoid group-wise operation issues
  mutate(Trek_flag = ifelse(is.na(Craving), 0, 1))

#if we want to remove the rows in which Trek is NA
signal_df <- signal_df %>%
  filter(Trek_flag == 1) 

#we finally remove all the NAs
signal_df = na.omit (signal_df)

#we remove the columns used for the missingness analysis
signal_df <- signal_df %>% select(-Trek_flag)

###########################################################################
###########################################################################
###                                                                     ###
###                    DUPLICATES AND VALIDITY CHECK                    ###
###                                                                     ###
###########################################################################
###########################################################################

#Checking for duplicated rows
duplicates <- duplicated(signal_df)

#we remove them
signal_df=signal_df[!duplicates,]

# Checking for duplicated timestamps per subject
duplicated_timestamps <- signal_df %>%
  group_by(Participant.Label) %>%
  filter(duplicated(Session.Scheduled.Time))

# Remove the duplicated timestamps
signal_df <- signal_df %>%
  group_by(Participant.Label) %>%
  filter(!duplicated(Session.Scheduled.Time))

rm(duplicated_timestamps)


############################################################################
############################################################################
###                                                                      ###
###                       ADDING THE NEW VARIABLES                       ###
###                                                                      ###
############################################################################
############################################################################

# make date column aspoxict

# Ensure your "Session.Scheduled.Time" is in POSIXct format
signal_df$Session.Scheduled.Time <- as.POSIXct(signal_df$Session.Scheduled.Time, format="%Y-%m-%d %H:%M:%OS")

## Here we want to create time blocks, such that we can merge with the event contingent data
# This means we lose some granularity in time information!

signal_df <- signal_df %>%
  mutate(TimeBlocks = case_when(
    hour(Session.Scheduled.Time) >= 6 & hour(Session.Scheduled.Time) < 8 ~ "Tussen 06:00 en 08:00",
    hour(Session.Scheduled.Time) >= 8 & hour(Session.Scheduled.Time) < 10 ~ "Tussen 08:00 en 10:00",
    hour(Session.Scheduled.Time) >= 10 & hour(Session.Scheduled.Time) < 12 ~ "Tussen 10:00 en 12:00",
    hour(Session.Scheduled.Time) >= 12 & hour(Session.Scheduled.Time) < 14 ~ "Tussen 12:00 en 14:00",
    hour(Session.Scheduled.Time) >= 14 & hour(Session.Scheduled.Time) < 16 ~ "Tussen 14:00 en 16:00",
    hour(Session.Scheduled.Time) >= 16 & hour(Session.Scheduled.Time) < 18 ~ "Tussen 16:00 en 18:00",
    hour(Session.Scheduled.Time) >= 18 & hour(Session.Scheduled.Time) < 20 ~ "Tussen 18:00 en 20:00",
    hour(Session.Scheduled.Time) >= 20 & hour(Session.Scheduled.Time) < 22 ~ "Tussen 20:00 en 22:00",
    TRUE ~ "Outside Defined Timeblocks" # for times outside the defined windows
  ))

#create a temporary column for hours 
signal_df <- signal_df %>%
  mutate(Hour = hour(Session.Scheduled.Time))

# create obsno
signal_df = signal_df %>%
  arrange(Participant.Label, Session.Scheduled.Time) %>%
  group_by(Participant.Label) %>%
  mutate(obsno = 1:n())

# create daycum
signal_df = signal_df %>%
  mutate(daycum = difftime(as.Date(Session.Scheduled.Time), as.Date(min(Session.Scheduled.Time, na.rm=TRUE)), units="days") + 1)

# Convert the daynr_study column to numeric
signal_df$day <- as.numeric(gsub(" days", "", signal_df$daycum))

# create beep

signal_df = signal_df[order(signal_df$Participant.Label, signal_df$Session.Scheduled.Time), ]
signal_df$beep = ave(rep(1, nrow(signal_df)), signal_df$Participant.Label, signal_df$daycum, FUN = seq_along)

#distinguish between week days and weekend
signal_df = signal_df %>% 
  mutate(WeekDay = ifelse(wday(Session.Scheduled.Time, week_start=1) %in% c(6,7), "weekEnd", "weekDay"))


############################################################################
############################################################################
###                                                                      ###
###                              DETRENDING                              ###
###                                                                      ###
############################################################################
############################################################################

#to de-trend for each subject

Vars <- colnames(signal_df)[4:14] # extracting all numerical variables
detrends <- matrix(nrow = length(unique(signal_df$Participant.Label)), ncol = length(Vars))

# Initialize a data frame for excluded subjects and variables
excluded_subjects <- data.frame(Participant = character(), Variable = character(), stringsAsFactors = FALSE)

# Convert columns in signal_df to double precision
signal_df[Vars] <- lapply(signal_df[Vars], as.double)

for (i in 1:length(unique(signal_df$Participant.Label))) {
  pp <- signal_df[signal_df$Participant.Label == unique(signal_df$Participant.Label)[i],]
  
  cat(paste0('\n\n\n\n\nWe are at subject ', unique(pp$Participant.Label), "  -> Row number in detrends is ", i, " \n\n"))
  
  for (v in 1:length(Vars)) {
    # Check if the current variable has only one unique value for the participant
    if (length(unique(pp[[Vars[v]]])) == 1) {
      cat(paste0('Skipping variable ', Vars[v], ' for participant ', unique(pp$Participant.Label), ' due to all values being the same.\n'))
      detrends[i,v] <- NA # 
      
      # Add the participant and variable to the excluded_subjects data frame
      excluded_subjects <- rbind(excluded_subjects, data.frame(Participant = unique(pp$Participant.Label), Variable = Vars[v]))
      next
    }
    
    # Construct the formula with backticks to handle special characters or spaces
    ff <- as.formula(paste0("`", Vars[[v]], "` ~ `Session.Scheduled.Time`"))
    fit <- lm(ff, data = pp)
    
    if (anova(fit)$P[1] < 0.05) {
      cat(paste0('DETRENDING FOR PARTICIPANT ', unique(pp$Participant.Label), " variable ", Vars[[v]], " -> column name in detrends is, ", v, "\n"))
      detrends[i,v] <- 1
      
      # adding mean to residuals
      updated_residuals <- residuals(fit) + mean(pp[[Vars[v]]][!is.na(pp[[Vars[[v]]]])])
      
      pp[[Vars[v]]][!is.na(pp[[Vars[[v]]]])] <- updated_residuals
      
      signal_df[signal_df$Participant.Label == unique(signal_df$Participant.Label)[i], Vars[v]] <- pp[,Vars[v]]
      
    } else {
      detrends[i,v] <- 0
    }
  }
 # Add a delay of 0.1 seconds (adjust as necessary)
  Sys.sleep(0.1)
}

rm(detrends,fit, pp, ff, i, v, Vars, excluded_subjects)
############################################################################
############################################################################
###                                                                      ###
###                              SUBSETTING                              ###
###                                                                      ###
############################################################################
############################################################################

##Now we filter the data to make two subsets 
#we write the new dataset as was processed until now into a csv file 
processed_path <- file.path (here("Networks","Network_data","Input_data", "Pre-processed_data", "dataframe_for_networks.csv"))
write.csv(signal_df, processed_path, row.names = FALSE)

rm(signal_df)

#Network estimation for the whole sample
# Set the output path for the filtered data
output_path <- here("Networks","Network_data","Input_data","All_Participants", "Filtered_data_whole")

# Define file paths
lifestyle_target1 <- file.path(output_path, "lifestyle_filtered_lowEE.csv")
lifestyle_target2 <- file.path(output_path, "lifestyle_filtered_highEE.csv")

# Check if the files exist
if (!file.exists(lifestyle_target1)|| !file.exists(lifestyle_target2)) {
  # Paths for the raw data files
  path_lifestyle_all <- here("Networks","Network_data","Input_data","Pre-processed_data", "dataframe_for_networks.csv")
  
  # Read the data
  data_lifestyle <- read.csv2(path_lifestyle_all, sep = ",")
  
  # Filter the data for a specific participant
  data_lifestyle_group1 <- subset(data_lifestyle, Participant.Label %in% c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066", "EoA071", "EoA081", "EoA097", "EoA110", "EoA037", "EoA057", "EoA056", "EoA070", "EoA033", "EoA061", "EoA073", "EoA080", "EoA010", "EoA087", "EoA011", "EoA014", "EoA032", "EoA039", "EoA086", "EoA004", "EoA036", "EoA055", "EoA096", "EoA018", "EoA048", "EoA103", "EoA007", "EoA064", "EoA105", "EoA106", "EoA109"))
  data_lifestyle_group2 <- subset(data_lifestyle, Participant.Label %in% c("EoA030", "EoA044", "EoA045", "EoA077", "EoA102", "EoA022", "EoA023", "EoA052", "EoA091", "EoA002", "EoA012", "EoA015", "EoA042", "EoA078", "EoA083", "EoA092", "EoA090", "EoA104", "EoA008", "EoA031", "EoA035", "EoA038", "EoA069", "EoA043", "EoA067", "EoA075", "EoA024", "EoA063", "EoA089", "EoA100", "EoA090", "EoA114", "EoA034", "EoA108", "EoA006", "EoA085", "EoA115", "EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EOA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019"))
  
  # Write the filtered data to CSV files
  write.csv(data_lifestyle_group1, lifestyle_target1, row.names = FALSE)
  write.csv(data_lifestyle_group2, lifestyle_target2, row.names = FALSE)
}

Low_EE_group = read.csv2 (here ("Networks","Network_data","Input_data","All_Participants", "Filtered_data_whole", "lifestyle_filtered_lowEE.csv"),sep = ",")
High_EE_group = read.csv2 (here ("Networks","Network_data","Input_data","All_Participants", "Filtered_data_whole", "lifestyle_filtered_highEE.csv"),sep = ",")

rm(data_lifestyle, data_lifestyle_group1, data_lifestyle_group2)

###########################################################################
###########################################################################
###                                                                     ###
###                        NETWORK PLOTS - MLVAR                        ###
###                                                                     ###
###########################################################################
###########################################################################
Vars<- c("Stressed", "Happy", "Sad", "Bored", "Craving")

#we cut from the dataframes the columns that we don't use anymore 
Low_EE_group <- Low_EE_group[, -c(4, 5, 10, 11, 12, 13)]
High_EE_group <- High_EE_group[, -c(4, 5, 10, 11, 12, 13)]

# Convert variables to double to ensure compatibility
Low_EE_group <- Low_EE_group %>%
  mutate(across(all_of(Vars), as.double))

High_EE_group <- High_EE_group %>%
  mutate(across(all_of(Vars), as.double))

mlVAR_LowEE <- mlVAR(Low_EE_group,
                     vars = Vars,
                     idvar = "Participant.Label",
                     dayvar = "day",
                     beepvar = "beep",
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     lags=1)

mlVAR_HighEE <- mlVAR(High_EE_group,
                      vars = Vars,
                      idvar = "Participant.Label",
                      dayvar = "day",
                      beepvar = "beep",
                      temporal = "orthogonal",
                      contemporaneous = "orthogonal",
                      lags=1)

#Contemporaneous
cont1 <- getNet(mlVAR_LowEE, "contemporaneous", nonsig = "hide", rule = "and")
cont2 <- getNet(mlVAR_HighEE, "contemporaneous", nonsig = "hide", rule = "and")

# Temporal:
temp1 <- getNet(mlVAR_LowEE, "temporal", nonsig = "hide")
temp2 <- getNet(mlVAR_HighEE, "temporal", nonsig = "hide")

# BETWEEN
bet1 <- getNet(mlVAR_LowEE, "between", nonsig = "hide", rule = "and")
bet2 <- getNet(mlVAR_HighEE, "between", nonsig = "hide", rule = "and")

#Vectors for the pies
means_vector_low <- apply(Low_EE_group[, 4:8], 2, mean, na.rm = TRUE)
means_vector_low <- unname(means_vector_low)
means_vector_low <- means_vector_low/100

means_vector_high <- apply(High_EE_group[, 4:8], 2, mean, na.rm = TRUE)
means_vector_high <- unname(means_vector_high)
means_vector_high <- means_vector_high/100

#Graphs for contemporaneous networks

jpeg(here("Networks","Networks_output","Estimation","Whole_sample","Contemporaneous_whole.jpg", 
     width=1200, height=600, res=300, quality=100)) 
# Set up the plotting area 
par(mfrow = c(1, 2))

#Plot:
qgraph(cont1,
       title = "Cont. Low EE",
       theme = "colorblind", labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_low)

qgraph(cont2,
       title = "Cont. High EE",
       theme = "colorblind", labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_high)

dev.off()  # Closes the JPEG device

#Graphs for temporal networks
plot.new()
jpeg(here("Networks","Networks_output","Estimation","Whole_sample","Temporary_whole.jpg", 
          width=1200, height=600, res=300, quality=100))
par(mfrow = c(1, 2))

qgraph(temp1,
       title = "Temp. Low EE",
       theme = "colorblind",
       labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_low)

qgraph(temp2,
       title = "Temp. High EE",
       theme = "colorblind",
       labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_high)

dev.off()  # Closes the JPEG device

#Graphs for between networks
plot.new()
jpeg(here("Networks","Networks_output","Estimation","Whole_sample","Between_whole.jpg", 
          width=1200, height=600, res=300, quality=100)) 
par(mfrow = c(1, 2))

qgraph(bet1,title = "Bet. Low EE", theme = "colorblind", labels = Vars, vsize = 13, asize = 5, mar = rep(6,4), layout = "circle", pie = means_vector_low)
qgraph(bet2,title = "Bet. High EE", theme = "colorblind", labels = Vars, vsize = 13, asize = 5, mar = rep(6,4), layout = "circle", pie = means_vector_high)

dev.off()  # Closes the JPEG device

rm(bet1, bet2, cont1, cont2, temp1, temp2, mlVAR_HighEE, mlVAR_LowEE, High_EE_group, Low_EE_group)

###########################################################################
###########################################################################
###                                                                     ###
###                     EXTREMES NETWORK ESTIMATION                     ###
###                                                                     ###
###########################################################################
###########################################################################

# Set the output path for the filtered data
output_path <- here("Networks","Network_data","Input_data","All_Participants", "Filtered_data_extremes")

# Define file paths
lifestyle_target1 <- file.path(output_path, "lifestyle_filtered_VlowEE.csv")
lifestyle_target2 <- file.path(output_path, "lifestyle_filtered_VhighEE.csv")

# Check if the files exist
if (!file.exists(lifestyle_target1)|| !file.exists(lifestyle_target2)) {
  # Paths for the raw data files
  path_lifestyle_all <- here("Networks","Network_data","Input_data","Pre-processed_data", "dataframe_for_networks.csv")
  
  # Read the data
  data_lifestyle <- read.csv2(path_lifestyle_all, sep = ",")
  
  #filtering for extremes 
  data_lifestyle_group1 <- subset(data_lifestyle, Participant.Label %in% c("EoA025", "EoA047", "EoA026", "EoA058", "EoA046", "EoA099", "EoA040", "EoA072", "EoA049", "EoA065", "EoA054", "EoA079", "EoA095", "EoA094", "EoA013", "EoA027", "EoA062", "EoA059", "EoA053", "EoA051", "EoA060", "EoA066"))
  data_lifestyle_group2 <- subset(data_lifestyle, Participant.Label %in% c("EoA001", "EoA009", "EoA020", "EoA113", "EoA016", "EoA041", "EoA068", "EoA076", "EoA093", "EoA088", "EoA028", "EoA082", "EoA084", "EoA098", "EoA003", "EoA101", "EoA074", "EoA111", "EoA107", "EoA021", "EoA029", "EoA017", "EoA019"))
  
  # Write the filtered data to CSV files
  write.csv(data_lifestyle_group1, lifestyle_target1, row.names = FALSE)
  write.csv(data_lifestyle_group2, lifestyle_target2, row.names = FALSE)
}

Low_EE_group = read.csv2 (here ("Networks","Network_data","Input_data","All_Participants", "Filtered_data_extremes", "lifestyle_filtered_VlowEE.csv"),sep = ",")
High_EE_group = read.csv2 (here ("Networks","Network_data","Input_data","All_Participants", "Filtered_data_extremes", "lifestyle_filtered_VhighEE.csv"),sep = ",")

rm(data_lifestyle, data_lifestyle_group1, data_lifestyle_group2)

###########################################################################
###########################################################################
###                                                                     ###
###                        NETWORK PLOTS - MLVAR                        ###
###                                                                     ###
###########################################################################
###########################################################################
Vars<- c("Stressed", "Happy", "Sad", "Bored", "Craving")

#we cut from the dataframes the columns that we don't use anymore 
Low_EE_group <- Low_EE_group[, -c(4, 5, 10, 11, 12, 13)]
High_EE_group <- High_EE_group[, -c(4, 5, 10, 11, 12, 13)]

# Convert variables to double to ensure compatibility
Low_EE_group <- Low_EE_group %>%
  mutate(across(all_of(Vars), as.double))

High_EE_group <- High_EE_group %>%
  mutate(across(all_of(Vars), as.double))

mlVAR_LowEE <- mlVAR(Low_EE_group,
                     vars = Vars,
                     idvar = "Participant.Label",
                     dayvar = "day",
                     beepvar = "beep",
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     lags=1)

mlVAR_HighEE <- mlVAR(High_EE_group,
                      vars = Vars,
                      idvar = "Participant.Label",
                      dayvar = "day",
                      beepvar = "beep",
                      temporal = "orthogonal",
                      contemporaneous = "orthogonal",
                      lags=1)

#Contemporaneous
cont1 <- getNet(mlVAR_LowEE, "contemporaneous", nonsig = "hide", rule = "and")
cont2 <- getNet(mlVAR_HighEE, "contemporaneous", nonsig = "hide", rule = "and")

# Temporal:
temp1 <- getNet(mlVAR_LowEE, "temporal", nonsig = "hide")
temp2 <- getNet(mlVAR_HighEE, "temporal", nonsig = "hide")

# BETWEEN
bet1 <- getNet(mlVAR_LowEE, "between", nonsig = "hide", rule = "and")
bet2 <- getNet(mlVAR_HighEE, "between", nonsig = "hide", rule = "and")

#Vectors for the pies
means_vector_low <- apply(Low_EE_group[, 4:8], 2, mean, na.rm = TRUE)
means_vector_low <- unname(means_vector_low)
means_vector_low <- means_vector_low/100

means_vector_high <- apply(High_EE_group[, 4:8], 2, mean, na.rm = TRUE)
means_vector_high <- unname(means_vector_high)
means_vector_high <- means_vector_high/100

#Graphs for contemporaneous networks

jpeg(here("Networks","Networks_output","Estimation","Extreme_groups","Contemporaneous_extremes.jpg", 
          width=1200, height=600, res=300, quality=100))
# Set up the plotting area 
par(mfrow = c(1, 2))

#Plot:
qgraph(cont1,
       title = "Cont. Low EE",
       theme = "colorblind", labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_low)

qgraph(cont2,
       title = "Cont. High EE",
       theme = "colorblind", labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_high)

dev.off()  # Closes the JPEG device

#Graphs for temporal networks
plot.new()
jpeg(here("Networks","Networks_output","Estimation","Extreme_groups","Temporal_extremes.jpg", 
          width=1200, height=600, res=300, quality=100)) 
par(mfrow = c(1, 2))

qgraph(temp1,
       title = "Temp. Low EE",
       theme = "colorblind",
       labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_low)

qgraph(temp2,
       title = "Temp. High EE",
       theme = "colorblind",
       labels = Vars,
       vsize = 13,
       asize = 5,
       mar = rep(6,4),
       layout = "circle",
       pie = means_vector_high)

dev.off()  # Closes the JPEG device

#Graphs for between networks
plot.new()
jpeg(here("Networks","Networks_output","Estimation","Extreme_groups","Between_extremes.jpg", 
          width=1200, height=600, res=300, quality=100))
par(mfrow = c(1, 2))

qgraph(bet1,title = "Bet. Low EE", theme = "colorblind", labels = Vars, vsize = 13, asize = 5, mar = rep(6,4), layout = "circle", pie = means_vector_low)
qgraph(bet2,title = "Bet. High EE", theme = "colorblind", labels = Vars, vsize = 13, asize = 5, mar = rep(6,4), layout = "circle", pie = means_vector_high)

dev.off()  # Closes the JPEG device

#Introducing all the references 
library(report)

References = report(sessionInfo())
ref_path = here("Networks","Networks_output","Estimation","SessionReferences.txt")
writeLines(References, con = ref_path)
