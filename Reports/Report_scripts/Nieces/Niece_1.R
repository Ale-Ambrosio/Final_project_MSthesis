#To pre-process EM_1

########### Packages Loading --------------------------------------------------------

library(skimr)
library(dplyr)
library(tidyr)
library(here)

#Before reading the raw data we need to concatenate the chunks of data downloaded from Avicenna
#Reading the files 
check_0_25 = read.csv2(here("Reports","Report_data","Input_data",'lifestyle_all_0-25.csv'), sep = ',')
check_26_50 = read.csv2(here("Reports","Report_data","Input_data",'lifestyle_all_26-50.csv'), sep = ',')
check_50_100 = read.csv2(here("Reports","Report_data","Input_data",'lifestyle_all_50-100.csv'), sep = ',')
check_101_129 = read.csv2(here("Reports","Report_data","Input_data",'lifestyle_all_101-129.csv'), sep = ',')

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
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that they have the same number of columns failed. This happened at line 30 in the Niece_n.R script.")
}

# Check column names of data frames
if (!all(names(check_0_25) == names(check_26_50) &
         names(check_26_50) == names(check_50_100) &
         names(check_50_100) == names(check_101_129))) {
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that all columns have the same name has failed. This happened at line 37 in the Niece_n.R script.")
}

EM1_df = bind_rows (check_0_25, check_26_50, check_50_100, check_101_129)

rm(check_0_25, check_26_50, check_50_100, check_101_129)

########### Get rid of metadata -----------------------------------------------------

# Identify columns containing 'metadata' in their names
metadata_cols <- grep("metadata", names(EM1_df), ignore.case = TRUE)


# Remove columns containing 'metadata' in EM1_df
EM1_df <- EM1_df[, -metadata_cols]

# remove metadata variable
rm(metadata_cols)
########### Change Variable Names ---------------------------------------------------


old_to_new_columns = c(
  
  'Participant.ID' = 'Participant.ID',
  'Participant.Label' = 'Participant.Label',
  'Session.Scheduled.Time' = 'Session.Scheduled.Time',
  
  'EM1_Cracker/beschuit/rijstwafel(met beleg)' = 'X.23_MAQ_1..Cracker.beschuit.rijstwafel..met.beleg.',
  'EM1_(Stok)brood(je)/boterham(met beleg)' = 'X.23_MAQ_2...Stok.brood.je..boterham..met.beleg.',
  'EM1_Zoet.brood(je)/pannenkoeken/poffertjes/wentelteefjes' = 'X.23_MAQ_3..Zoet.brood.je..pannenkoeken.poffertjes.wentelteefj',
  'EM1_Ontbijtpap/havermout/muesli(met/zonder.melk)' = 'X.23_MAQ_4..Ontbijtpap.havermout.muesli..met.zonder.melk.',
  'EM1_Cornflakes/zoete.ontbijtgranen(met/zonder.melk)' = 'X.23_MAQ_5..Cornflakes.zoete.ontbijtgranen..met.zonder.melk.',
  'EM1_Cruesli/granola(met/zonder.melk)' = 'X.23_MAQ_6..Cruesli.granola..met.zonder.melk.',
  
  'EM1_(Plantaardige)yoghurt/kwark' = 'X.24_MAQ_1...Plantaardige..yoghurt.kwark',
  'EM1_Ei' = 'X.24_MAQ_2..Ei',
  
  'EM1_Fruit/fruitsalade' = 'X.25_MAQ_1..Fruit.fruitsalade',
  'EM1_(Snoep/snack)groente' = 'X.25_MAQ_2...Snoep.snack.groente',
  'EM1_Bijgerecht.salade' = 'X.25_MAQ_3..Bijgerecht.salade',
  'EM1_Gedroogde.vruchten' = 'X.25_MAQ_4..Gedroogde.vruchten',
  
  'EM1_Aardappel-groente-vlees/vis/vleesvervanger' = 'X.26_MAQ_1..Aardappel.groente.vlees.vis.vleesvervanger',
  'EM1_Rijst/noedel/pasta-gerecht' = 'X.26_MAQ_2..Rijst.noedel.pasta.gerecht',
  'EM1_Couscous/bulgur/quinoa-gerecht' = 'X.26_MAQ_3..Couscous.bulgur.quinoa.gerecht',
  'EM1_Gevuld broodje/wrap/taco/pita' = 'X.26_MAQ_4..Gevuld.broodje.wrap.taco.pita',
  'EM1_Quiche/hartige.taart/zelfgemaakte.pizza' = 'X.26_MAQ_5..Quiche.hartige.taart.zelfgemaakte.pizza',
  'EM1_Sushi/tapas' = 'X.26_MAQ_6..Sushi.tapas',
  'EM1_Friet/gefrituurde.snack/pizza' = 'X.26_MAQ_7..Friet.gefrituurde.snack.pizza',
  'EM1_Maaltijdsalade' = 'X.26_MAQ_8..Maaltijdsalade',
  
  'EM1_Granenreep/proteïnereep/ontbijtkoek/fruitbiscuit' = 'X.27_MAQ_1..Granenreep.proteïnereep.ontbijtkoek.fruitbiscuit',
  'EM1_Koekje/wafel' = 'X.27_MAQ_2..Koekje.wafel',
  'EM1_Chips/gezouten.toastjes.of.noten/popcorn' = 'X.27_MAQ_3..Chips.gezouten.toastjes.of.noten.popcorn',
  'EM1_Chocolade(reep)/M&Ms/snoep' = 'X.27_MAQ_4..Chocolade.reep..M.Ms.snoep',
  'EM1_Ongezouten.noten/pitten/zaden' = 'X.27_MAQ_5..Ongezouten.noten.pitten.zaden',
  'EM1_Gefrituurde.snack' = 'X.27_MAQ_6..Gefrituurde.snack',
  'EM1_Kaasjes/worstjes' = 'X.27_MAQ_7..Kaasjes.worstjes',
  
  'EM1_Taart/gebak/vlaai/bonbons/soesjes' = 'X.28_MAQ_1..Taart.gebak.vlaai.bonbons.soesjes',
  'EM1_Roomijs/ijstaart' = 'X.28_MAQ_2..Roomijs.ijstaart',
  'EM1_Vla/pudding/dessert' = 'X.28_MAQ_3..Vla.pudding.dessert',
  'EM1_Waterijs/sorbetijs' = 'X.28_MAQ_4..Waterijs.sorbetijs',
  'EM1_Kaasplank' = 'X.28_MAQ_5..Kaasplank',
  
  'EatingPlan' = 'X.29_SAQ..Eetmoment.plan',
  
  'Categ_EMOTIE_Bang/angstig' = 'X.30_MAQ_1..Bang.angstig',
  'Categ_EMOTIE_Geïrriteerd/boos' = 'X.30_MAQ_2..Geïrriteerd.boos',
  'Categ_EMOTIE_Gestrest' = 'X.30_MAQ_3..Gestrest',
  'Categ_EMOTIE_Ontspannen/kalm' = 'X.30_MAQ_4..Ontspannen.kalm',
  'Categ_EMOTIE_Blij/vrolijk' = 'X.30_MAQ_5..Blij.vrolijk',
  'Categ_EMOTIE_Droevig/somber' = 'X.30_MAQ_6..Droevig.somber',
  'Categ_EMOTIE_Verveeld' = 'X.30_MAQ_7..Verveeld',
  'Categ_EMOTIE_Vermoeid' = 'X.30_MAQ_8..Vermoeid',
  'Categ_EMOTIE_Energiek' = 'X.30_MAQ_9..Energiek',
  'Categ_EMOTIE_Zelfverzekerd' = 'X.30_MAQ_10..Zelfverzekerd',
  'Categ_EMOTIE_Onzeker' = 'X.30_MAQ_11..Onzeker',
  'Categ_EMOTIE_Beschaamd/schuldig' = 'X.30_MAQ_12..Beschaamd.schuldig',
  'Categ_EMOTIE_Eenzaam' = 'X.30_MAQ_13..Eenzaam',
  
  'WIE_Niemand' = 'X.31_MAQ_1..Niemand',
  'WIE_Partner' = 'X.31_MAQ_2..Partner',
  'WIE_Gezinsleden/huisgenoten' = 'X.31_MAQ_3..Gezinsleden.huisgenoten',
  'WIE_Collegas/klasgenoten' = 'X.31_MAQ_4..Collega.s.klasgenoten',
  'WIE_Onbekenden' = 'X.31_MAQ_5..Onbekenden',
  'WIE_Anders' = 'X.31_MAQ_6..Anders',
  'WIE_Vrienden' = 'X.31_MAQ_7..Vrienden',
  'WIE_Overige.familieleden' = 'X.31_MAQ_8..Overige.familieleden',
  
  'EatmomentWaar' = 'X.32_SAQ..Eetmoment..waar',
  
  'ACTIVITY_Nee' = 'X.33_MAQ_1..Nee',
  'ACTIVITY_Lezen(boek,krant,nieuws)' = 'X.33_MAQ_2..Lezen..boek.krant.nieuws.',
  'ACTIVITY_Scrollen.op.social.media' = 'X.33_MAQ_3..Scrollen.op.social.media',
  'ACTIVITY_TV.kijken' = 'X.33_MAQ_4..TV.kijken',
  'ACTIVITY_Spelletje.spelen' = 'X.33_MAQ_5..Spelletje.spelen',
  'ACTIVITY_Werken/studeren' = 'X.33_MAQ_6..Werken.studeren',
  'ACTIVITY_Praten' = 'X.33_MAQ_7..Praten',
  'ACTIVITY_Anders' = 'X.33_MAQ_8..Anders',
  'ACTIVITY_Koken' = 'X.33_MAQ_9..Koken',
  
  'EM1_Soep' = 'X.179_MAQ_1..Soep',
  'EM1_Gevulde.soep/maaltijdsoep' = 'X.179_MAQ_2..Gevulde.soep.maaltijdsoep',
  ## Addition Tijd 19 - March !
  'EatmomentTijd' = 'X.109_SAQ..Eetmoment.tijd',
  
  'Intensity_EMOTIE_Bang' = 'X.114_VAS..Eetmoment.Intensiteit.bang',
  'Intensity_EMOTIE_Geirriteerd' = 'X.115_VAS..Eetmoment.intensiteit.geïrriteerd',
  'Intensity_EMOTIE_Gestrest' = 'X.116_VAS..Eetmoment.intensiteit.gestrest',
  'Intensity_EMOTIE_Kalm' = 'X.117_VAS..Eetmoment.intensiteit.ontspannen',
  'Intensity_EMOTIE_Blij' = 'X.118_VAS..Eetmoment.intensiteit.blij',
  'Intensity_EMOTIE_Droevig' = 'X.119_VAS..Eetmoment.intensiteit.droevig',
  'Intensity_EMOTIE_Verveeld' = 'X.120_VAS..Eetmoment.intensiteit.verveeld',
  'Intensity_EMOTIE_Vermoeid' = 'X.121_VAS..Eetmoment.intensiteit.vermoeid',
  'Intensity_EMOTIE_Energiek' = 'X.122_VAS..Eetmoment.intensiteit.energiek',
  'Intensity_EMOTIE_Zelfverzekerd' = 'X.123_VAS..Eetmoment.intensiteit.zelfverzekerd',
  'Intensity_EMOTIE_Onzeker' = 'X.124_VAS..Eetmoment.intensiteit.onzeker',
  'Intensity_EMOTIE_Beschaamd' = 'X.125_VAS..Eetmoment.intensiteit.beschaamd',
  'Intensity_EMOTIE_Eenzam' = 'X.126_VAS..Eetmoment.intensiteit.eenzaam')



# now renaming

EM1_df = EM1_df %>% rename(all_of(old_to_new_columns))
# Now cutting EM1_df to only the renamed columns above

# Extract the renamed column names
renamed_columns <- names(old_to_new_columns)

# Subset the EM_ data frame to keep only the renamed columns
EM1_df <- EM1_df %>% select(any_of((renamed_columns)))

rm(renamed_columns, old_to_new_columns)

########### Checking for Duplicates -------------------------------------------------
#Checking for duplicated rows
duplicates <- duplicated(EM1_df)

#we remove them
EM1_df=EM1_df[!duplicates,]

# sanity check to make sure all variable names are unique

if (any(duplicated(names(EM1_df))) != FALSE) {
  stop("ERROR! Duplicated column names found in the first EM data frame! This happened at line 183, Script Niece_2.R.")
}

########### Make long format by EM_ (food categories) -------------------------------


EM1_df_long <- EM1_df %>%
  pivot_longer(cols = starts_with("EM"),
               names_to = c("EatingMomentNumber", "FoodSubcategory"),
               names_sep = "_",
               values_to = "value")


########### RM stuff i dont need

rm(EM1_df)
