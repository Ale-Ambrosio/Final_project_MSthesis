#Main pre-processing for report 

##### Packages ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(xtable)
library(stargazer)
library(here)

##### Concat vertically all the EM data frames (1-5) --------------------------


source(here("Reports","Report_scripts", "Nieces","Niece_1.R"))

source(here("Reports","Report_scripts", "Nieces","Niece_2.R"))

source(here("Reports","Report_scripts", "Nieces","Niece_3.R"))

source(here("Reports","Report_scripts", "Nieces","Niece_4.R"))

source(here("Reports","Report_scripts", "Nieces","Niece_5.R"))
##### SECURITY CHECKS BEFORE MERGING VERTICALLY THE DATA FRAMES

# If anything fails in this section, the code will throw an error message

# Before concat vertically, check a few things related to the data frames, if these tests dont work throw errors!

# Check dimensions of data frames
if (!all(dim(EM1_df_long) == dim(EM2_df_long) & dim(EM2_df_long) == dim(EM3_df_long) & dim(EM3_df_long) == dim(EM4_df_long) & dim(EM4_df_long) == dim(EM5_df_long))) {
  stop("ERROR! When concat vertically the EM_long individual eating moment data frames, the check that all EM_longs have the same dimmension has failed. This happened at line 31 in the Main_Child.R script.")
}

# Check column names of data frames
if (!all(names(EM1_df_long) == names(EM2_df_long) &
         names(EM2_df_long) == names(EM3_df_long) &
         names(EM3_df_long) == names(EM4_df_long) &
         names(EM4_df_long) == names(EM5_df_long))) {
  stop("ERROR! When concat vertically the EM_long individual eating moment data frames, the check that all EM_longs have the same name has failed. This happened at line 39 in the Main_Child.R script.")
}


# If we got to this step, means all is good, and we can concat vertically.

##### Concat vertically

EM_all_long = bind_rows(EM1_df_long, EM2_df_long, EM3_df_long, EM4_df_long, EM5_df_long) # keep in mind that this bind_rows would concat in the order of the EM, so first all EM1, then EM2, etc. This means that they are not sorted by 'Session.Date', but by EatingMoment number.

#cleaning workspace
rm(EM1_df_long, EM2_df_long, EM3_df_long, EM4_df_long, EM5_df_long)

##### Fast Cleaning - Removing all the rows without food information ----------

# Remember that the original raw data frame had a column for each individual food category.
# And as we did not preprocessing and converted to long format, each food subcategory would have a row.
# But of course, the subject would not eat every food subcategory at every time point.
# Therefore, every food subcategory that has 'NAN' it means they either did not complete or was not registered at that beep, so we can ignore.


EM_all_long$value = as.factor(EM_all_long$value)

EM_all_long <- EM_all_long %>%
  filter(!is.na(value) & value != "")

# If it has less than 10 observations, don't run more than this part.
# Check if any participant has less than 10 observations
participants_with_few_observations <- EM_all_long %>%
  group_by(Participant.Label) %>%
  summarise(count = n()) %>%
  filter(count < 10)

if (nrow(participants_with_few_observations) > 0) {
  participants <- paste(participants_with_few_observations$Participant.Label, collapse = ", ")
  stop(paste("Error: The data frame EM_all_long (with food subcategories) has less than 10 observations for the following participant(s):", participants, ". Error happened at line 74 of the Main_Child.R."))
}

rm(participants_with_few_observations)

## Security check. After this stage, we should be left with just the food subcategories that were listed.
# Therefore, all the entries in the "value" column should be "true" (not boolean TRUE, but "true").
# Check this, otherwise throw error.

# Check if all values in the "value" column are "true"
if (!all(EM_all_long$value == 'true')) {
  stop("In the EM_all_long Not all values in the 'value' column are 'true'. Please ensure all values are 'true'. Error occured in the checking from line 83 of Main_Child.R).")
}

#removing columns we won't use
EM_all_long <- EM_all_long %>% select(-EatingPlan, -EatmomentWaar)

cols_to_remove <- grep("^WIE_", names(EM_all_long), value = TRUE)
EM_all_long <- EM_all_long[ , !(names(EM_all_long) %in% cols_to_remove)]

cols_to_remove2 <- grep("^ACTIVITY_", names(EM_all_long), value = TRUE)
EM_all_long <- EM_all_long[ , !(names(EM_all_long) %in% cols_to_remove2)]

rm(cols_to_remove, cols_to_remove2)
rm(duplicates, pps_to_remove_1, pps_to_remove_2)


##### NAN handling of the other rows ------------------------------------------

# There are multiple types of NAN values in the data frame, so handling them has to be done carefully.
# Below I explain my logic.

# Now all the columns that are single choice, missing value would be NAN. We would handle those now.
# Get column names containing "_" and columns without underscore
# The logic here as it stands is that all the columns with "_" should be one-hot-encoded, and the others shouldnt be hot-encoded. This is based on how I renamed the variables in the "EM_#scripts".

hot_encoded_cols <- grep("_", names(EM_all_long), value = TRUE)
columns_without_underscore <- names(EM_all_long)[!(names(EM_all_long) %in% hot_encoded_cols)]

#we throw an error if there is a NA in the columns_without_underscore
na_check <- is.na(EM_all_long[, columns_without_underscore])

all_na_check_false <- all(!na_check)

if (!all_na_check_false) {
  stop("Error: Not all values are FALSE in the NA check. This error occurred at line 119 in the Main_Child.R script")
}

rm(na_check, all_na_check_false)

# Some security checks. Let's check for each column in "columns_without_underscore" that the values in them is in the acceptable range.
## First Eating Moment Number

# Check if all values in the column are in the accepted_values vector
if(!all(EM_all_long$EatingMomentNumber %in% c("EM1", "EM2", "EM3", "EM4", "EM5"))) {
  stop("Main_Child.R script (line 129) - Error: The 'EatingMomentNumber' column contains invalid values.")
}

# Foodsubcategory will check later, value we already checked, for session scheduled time there can be duplicates as we have long format.



## IMPORTANT ! Observed on Wed, 17th of Jan that this will replace "" with NAN, but then in the plots
# If that variable within columns_without_underscore has NAN it will be placed in the graph, which is not nice.
# What I'll implement - for each plot_df I make for the graphs, remove rows with NAN in the variable that matters.

# changing hot-encoded values of '' and NA with 0, and true with 1 in columns with underscore
EM_all_long <- mutate_at(EM_all_long, hot_encoded_cols, ~ ifelse(. == "" | is.na(.), 0, ifelse(. == "true", 1, .)))

# Security check - at this point, all values in hot_encoded_cols should be 0 or 1, otherwise throw error.
#I remove from this checking the columns with Emotions Intensity
hot_encoded_cols <- hot_encoded_cols[!grepl("^Intensity_", hot_encoded_cols)]

# Check if all entries in the specified columns are either 0 or 1
check_values <- function(data, columns) {
  for (col in columns) {
    if (any(data[[col]] != 0 & data[[col]] != 1)) {
      stop(paste("Column", col, "contains values other than 0 or 1. Error found at line 151 in the Main_Child.R"))
    }
  }
}
# Check the values in the specified columns
check_values(EM_all_long, hot_encoded_cols)

rm(check_values)

##### Removing more columns - EM number and 'value' column --------------------------------
# These columns are not needed anymore. We already removed all the ones that have 'NAN' at value = invalid food subcategories.
# And the eating moment number is not relevant, as we don't care about what eating moment number this happened at.

EM_all_long <- EM_all_long[, !(names(EM_all_long) == "value")]
EM_all_long <- EM_all_long[, !(names(EM_all_long) == "EatingMomentNumber")]

# Remember that i will need the columns_without_underscore and hot_encoded_cols later, when i turn into factors.
# I don't rm() hot_encoded_cols now therefore, but I do that with columns without underscore, see below why.
rm(columns_without_underscore) # I am rming this here because later I am removing some of the columns that i want later to be stored in this variable for making factors

###### Define Food Labels ------------------------------------------------------
# This is based on our food labels definitions.


green_list = c('Ontbijtpap/havermout/muesli(met/zonder.melk)',
               '(Plantaardige)yoghurt/kwark',
               'Ei',
               'Fruit/fruitsalade',
               '(Snoep/snack)groente',
               'Bijgerecht.salade',
               'Soep',
               'Gevulde.soep/maaltijdsoep',
               'Couscous/bulgur/quinoa-gerecht',
               'Maaltijdsalade',
               'Ongezouten.noten/pitten/zaden')

gray_list = c('Cracker/beschuit/rijstwafel(met beleg)',
              '(Stok)brood(je)/boterham(met beleg)',
              'Cruesli/granola(met/zonder.melk)',
              'Gedroogde.vruchten',
              'Aardappel-groente-vlees/vis/vleesvervanger',
              'Rijst/noedel/pasta-gerecht',
              'Gevuld broodje/wrap/taco/pita',
              'Quiche/hartige.taart/zelfgemaakte.pizza',
              'Sushi/tapas',
              'Granenreep/proteïnereep/ontbijtkoek/fruitbiscuit',
              'Waterijs/sorbetijs')
# Here we have the 'gray' category, that is neither red or green for plots

red_list = c('Zoet.brood(je)/pannenkoeken/poffertjes/wentelteefjes',
             'Cornflakes/zoete.ontbijtgranen(met/zonder.melk)',
             'Friet/gefrituurde.snack/pizza',
             'Koekje/wafel',
             'Chips/gezouten.toastjes.of.noten/popcorn',
             'Chocolade(reep)/M&Ms/snoep',
             'Gefrituurde.snack',
             'Kaasjes/worstjes',
             'Taart/gebak/vlaai/bonbons/soesjes',
             'Roomijs/ijstaart',
             'Vla/pudding/dessert',
             'Kaasplank' )

##### Making a column with the food labels ------------------------------------
# Now we will add a column in our data frame based on this.

EM_all_long <- EM_all_long %>%
  mutate(
    Label = case_when(
      FoodSubcategory %in% green_list ~ "Green Label",
      FoodSubcategory %in% gray_list ~ "Gray Label",
      FoodSubcategory %in% red_list ~ "Red Label",
      TRUE ~ "Geen Label"  # Assign a default label if not found in any list
    )
  )

# Check if "Geen Label" exists in the "FoodSubcategory" column
if ("Geen Label" %in% EM_all_long$FoodSubcategory) {
  stop("Error Main_Child.R script (line 228): 'Geen Label' found in the FoodSubcategory column. Please check your data.")
}

##### Prepare for plotting --------------------------------
# Before plotting, we need to make sure our columns are in the right format.
# First, the columns_without_underscore should be factors with different levels.

hot_encoded_cols <- grep("_", names(EM_all_long), value = TRUE)
columns_without_underscore <- names(EM_all_long)[!(names(EM_all_long) %in% hot_encoded_cols)]

EM_all_long[columns_without_underscore] = lapply(EM_all_long[columns_without_underscore], factor)

# Assuming hot_encoded_cols contains column names that need to be converted to numeric
# Basically all our hot-encoded-cols should be numbers, which makes it easier for later processing.

EM_all_long <- EM_all_long %>%
  mutate_at(vars(hot_encoded_cols), as.numeric)


rm(columns_without_underscore, hot_encoded_cols) # Now we can rm() in peace everything, so only "EM_all_long", fully preprocessed, is left.

# Check AGAIN if the data frame has less than 10 observations for any participant

# Check if any participant has less than 10 observations
participants_with_few_observations <- EM_all_long %>%
  group_by(Participant.Label) %>%
  summarise(count = n()) %>%
  filter(count < 10)

if (nrow(participants_with_few_observations) > 0) {
  participants <- paste(participants_with_few_observations$Participant.Label, collapse = ", ")
  stop(paste("Error: The data frame EM_all_long (with food subcategories) has less than 10 observations for the following participant(s):", participants, ". Error happened at line 259 (Main_Child.R)."))
}

rm(participants_with_few_observations)

#we keep a copy of the pre-processed dataframe
proc_path = file.path (here("Reports","Report_data","Pre_processed_df", "Pre_processed_dataframe.csv"))
write.csv(EM_all_long, proc_path, row.names = FALSE)
