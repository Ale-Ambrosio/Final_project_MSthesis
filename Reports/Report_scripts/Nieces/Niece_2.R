#Niece 2

########### Packages Loading --------------------------------------------------------

library(skimr)
library(dplyr)
library(tidyr)
library(here)

########### Reading DF --------------------------------------------------------------
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
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that they have the same number of columns failed. This happened at line 31 in the Niece_n.R script.")
}

# Check column names of data frames
if (!all(names(check_0_25) == names(check_26_50) &
         names(check_26_50) == names(check_50_100) &
         names(check_50_100) == names(check_101_129))) {
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that all columns have the same name has failed. This happened at line 38 in the Niece_n.R script.")
}

EM2_df = bind_rows (check_0_25, check_26_50, check_50_100, check_101_129)

rm(check_0_25, check_26_50, check_50_100, check_101_129)

########### Get rid of metadata -----------------------------------------------------

# Identify columns containing 'metadata' in their names
metadata_cols <- grep("metadata", names(EM2_df), ignore.case = TRUE)


########### Remove columns containing 'metadata' both in the original_df and EM2_df
EM2_df <- EM2_df[, -metadata_cols]

########### Remove metadata variable
rm(metadata_cols)


########### Change Variable Names ---------------------------------------------------

old_to_new_columns = c(
  
  'Participant.ID' = 'Participant.ID',
  'Participant.Label' = 'Participant.Label',
  'Session.Scheduled.Time' = 'Session.Scheduled.Time',
  
  'EM2_Cracker/beschuit/rijstwafel(met beleg)' = 'X.38_MAQ_1..Cracker.beschuit.rijstwafel..met.beleg.',
  'EM2_(Stok)brood(je)/boterham(met beleg)' = 'X.38_MAQ_2...Stok.brood.je..boterham..met.beleg.',
  'EM2_Zoet.brood(je)/pannenkoeken/poffertjes/wentelteefjes' = 'X.38_MAQ_3..Zoet.brood.je..pannenkoeken.poffertjes.wentelteefj',
  'EM2_Ontbijtpap/havermout/muesli(met/zonder.melk)' = 'X.38_MAQ_4..Ontbijtpap.havermout.muesli..met.zonder.melk.',
  'EM2_Cornflakes/zoete.ontbijtgranen(met/zonder.melk)' = 'X.38_MAQ_5..Cornflakes.zoete.ontbijtgranen..met.zonder.melk.',
  'EM2_Cruesli/granola(met/zonder.melk)' = 'X.38_MAQ_6..Cruesli.granola..met.zonder.melk.',
  
  'EM2_(Plantaardige)yoghurt/kwark' = 'X.39_MAQ_1...Plantaardige..yoghurt.kwark',
  'EM2_Ei' = 'X.39_MAQ_2..Ei',
  
  'EM2_Fruit/fruitsalade' = 'X.40_MAQ_1..Fruit.fruitsalade',
  'EM2_(Snoep/snack)groente' = 'X.40_MAQ_2...Snoep.snack.groente',
  'EM2_Bijgerecht.salade' = 'X.40_MAQ_3..Bijgerecht.salade',
  'EM2_Gedroogde.vruchten' = 'X.40_MAQ_4..Gedroogde.vruchten',
  
  'EM2_Aardappel-groente-vlees/vis/vleesvervanger' = 'X.41_MAQ_1..Aardappel.groente.vlees.vis.vleesvervanger',
  'EM2_Rijst/noedel/pasta-gerecht' = 'X.41_MAQ_2..Rijst.noedel.pasta.gerecht',
  'EM2_Couscous/bulgur/quinoa-gerecht' = 'X.41_MAQ_3..Couscous.bulgur.quinoa.gerecht',
  'EM2_Gevuld broodje/wrap/taco/pita' = 'X.41_MAQ_4..Gevuld.broodje.wrap.taco.pita',
  'EM2_Quiche/hartige.taart/zelfgemaakte.pizza' = 'X.41_MAQ_5..Quiche.hartige.taart.zelfgemaakte.pizza',
  'EM2_Sushi/tapas' = 'X.41_MAQ_6..Sushi.tapas',
  'EM2_Friet/gefrituurde.snack/pizza' = 'X.41_MAQ_7..Friet.gefrituurde.snack.pizza',
  'EM2_Maaltijdsalade' = 'X.41_MAQ_8..Maaltijdsalade',
  
  'EM2_Granenreep/proteïnereep/ontbijtkoek/fruitbiscuit' = 'X.42_MAQ_1..Granenreep.proteïnereep.ontbijtkoek.fruitbiscuit',
  'EM2_Koekje/wafel' = 'X.42_MAQ_2..Koekje.wafel',
  'EM2_Chips/gezouten.toastjes.of.noten/popcorn' = 'X.42_MAQ_3..Chips.gezouten.toastjes.of.noten.popcorn',
  'EM2_Chocolade(reep)/M&Ms/snoep' = 'X.42_MAQ_4..Chocolade.reep..M.Ms.snoep',
  'EM2_Ongezouten.noten/pitten/zaden' = 'X.42_MAQ_5..Ongezouten.noten.pitten.zaden',
  'EM2_Gefrituurde.snack' = 'X.42_MAQ_6..Gefrituurde.snack',
  'EM2_Kaasjes/worstjes' = 'X.42_MAQ_7..Kaasjes.worstjes',
  
  'EM2_Taart/gebak/vlaai/bonbons/soesjes' = 'X.43_MAQ_1..Taart.gebak.vlaai.bonbons.soesjes',
  'EM2_Roomijs/ijstaart' = 'X.43_MAQ_2..Roomijs.ijstaart',
  'EM2_Vla/pudding/dessert' = 'X.43_MAQ_3..Vla.pudding.dessert',
  'EM2_Waterijs/sorbetijs' = 'X.43_MAQ_4..Waterijs.sorbetijs',
  'EM2_Kaasplank' = 'X.43_MAQ_5..Kaasplank',
  
  'EatingPlan' = 'X.44_SAQ..Eetmoment.plan',
  
  'Categ_EMOTIE_Bang/angstig' = 'X.45_MAQ_1..Bang.angstig',
  'Categ_EMOTIE_Geïrriteerd/boos' = 'X.45_MAQ_2..Geïrriteerd.boos',
  'Categ_EMOTIE_Gestrest' = 'X.45_MAQ_3..Gestrest',
  'Categ_EMOTIE_Ontspannen/kalm' = 'X.45_MAQ_4..Ontspannen.kalm',
  'Categ_EMOTIE_Blij/vrolijk' = 'X.45_MAQ_5..Blij.vrolijk',
  'Categ_EMOTIE_Droevig/somber' = 'X.45_MAQ_6..Droevig.somber',
  'Categ_EMOTIE_Verveeld' = 'X.45_MAQ_7..Verveeld',
  'Categ_EMOTIE_Vermoeid' = 'X.45_MAQ_8..Vermoeid',
  'Categ_EMOTIE_Energiek' = 'X.45_MAQ_9..Energiek',
  'Categ_EMOTIE_Zelfverzekerd' = 'X.45_MAQ_10..Zelfverzekerd',
  'Categ_EMOTIE_Onzeker' = 'X.45_MAQ_11..Onzeker',
  'Categ_EMOTIE_Beschaamd/schuldig' = 'X.45_MAQ_12..Beschaamd.schuldig',
  'Categ_EMOTIE_Eenzaam' = 'X.45_MAQ_13..Eenzaam',
  
  'WIE_Niemand' = 'X.46_MAQ_1..Niemand',
  'WIE_Partner' = 'X.46_MAQ_2..Partner',
  'WIE_Gezinsleden/huisgenoten' = 'X.46_MAQ_3..Gezinsleden.huisgenoten',
  'WIE_Collegas/klasgenoten' = 'X.46_MAQ_4..Collega.s.klasgenoten',
  'WIE_Onbekenden' = 'X.46_MAQ_5..Onbekenden',
  'WIE_Anders' = 'X.46_MAQ_6..Anders',
  'WIE_Vrienden' = 'X.46_MAQ_7..Vrienden',
  'WIE_Overige.familieleden' = 'X.46_MAQ_8..Overige.familieleden',
  
  'EatmomentWaar' = 'X.47_SAQ..Eetmoment..waar',
  
  'ACTIVITY_Nee' = 'X.48_MAQ_1..Nee',
  'ACTIVITY_Lezen(boek,krant,nieuws)' = 'X.48_MAQ_2..Lezen..boek.krant.nieuws.',
  'ACTIVITY_Scrollen.op.social.media' = 'X.48_MAQ_3..Scrollen.op.social.media',
  'ACTIVITY_TV.kijken' = 'X.48_MAQ_4..TV.kijken',
  'ACTIVITY_Spelletje.spelen' = 'X.48_MAQ_5..Spelletje.spelen',
  'ACTIVITY_Werken/studeren' = 'X.48_MAQ_6..Werken.studeren',
  'ACTIVITY_Praten' = 'X.48_MAQ_7..Praten',
  'ACTIVITY_Anders' = 'X.48_MAQ_8..Anders',
  'ACTIVITY_Koken' = 'X.48_MAQ_9..Koken',
  
  'EM2_Soep' = 'X.180_MAQ_1..Soep',
  'EM2_Gevulde.soep/maaltijdsoep' = 'X.180_MAQ_2..Gevulde.soep.maaltijdsoep',
  ## Addition tijd 19 - March !
  'EatmomentTijd' = 'X.110_SAQ..Eetmoment.tijd',
  
  'Intensity_EMOTIE_Bang' = 'X.127_VAS..Eetmoment.Intensiteit.bang',
  'Intensity_EMOTIE_Geirriteerd' = 'X.128_VAS..Eetmoment.intensiteit.geïrriteerd',
  'Intensity_EMOTIE_Gestrest' = 'X.129_VAS..Eetmoment.intensiteit.gestrest',
  'Intensity_EMOTIE_Kalm' = 'X.139_VAS..Eetmoment.intensiteit.ontspannen',
  'Intensity_EMOTIE_Blij' = 'X.143_VAS..Eetmoment.intensiteit.blij',
  'Intensity_EMOTIE_Droevig' = 'X.147_VAS..Eetmoment.intensiteit.droevig',
  'Intensity_EMOTIE_Verveeld' = 'X.151_VAS..Eetmoment.intensiteit.verveeld',
  'Intensity_EMOTIE_Vermoeid' = 'X.155_VAS..Eetmoment.intensiteit.vermoeid',
  'Intensity_EMOTIE_Energiek' = 'X.159_VAS..Eetmoment.intensiteit.energiek',
  'Intensity_EMOTIE_Zelfverzekerd' = 'X.163_VAS..Eetmoment.intensiteit.zelfverzekerd',
  'Intensity_EMOTIE_Onzeker' = 'X.167_VAS..Eetmoment.intensiteit.onzeker',
  'Intensity_EMOTIE_Beschaamd' = 'X.171_VAS..Eetmoment.intensiteit.beschaamd',
  'Intensity_EMOTIE_Eenzam' = 'X.175_VAS..Eetmoment.intensiteit.eenzaam')


# Now renaming

EM2_df = EM2_df %>% rename(old_to_new_columns)

# Now cutting EM2_df to only the renamed columns above

# Extract the renamed column names
renamed_columns <- names(old_to_new_columns)

# Subset the EM_ data frame to keep only the renamed columns
EM2_df <- EM2_df %>% select(any_of((renamed_columns)))

rm(renamed_columns, old_to_new_columns)

########### Checking for Duplicates -------------------------------------------------

#Checking for duplicated rows
duplicates <- duplicated(EM2_df)

#we remove them
EM2_df=EM2_df[!duplicates,]

# sanity check to make sure all variable names are unique

if (any(duplicated(names(EM2_df))) != FALSE) {
  stop("ERROR! Duplicated column names found in the second EM data frame! This happened at line 186, Script Niece_2.R.")
}

########### Make long format by EM_ (food categories) -------------------------------

EM2_df_long <- EM2_df %>%
  pivot_longer(cols = starts_with("EM"),
               names_to = c("EatingMomentNumber", "FoodSubcategory"),
               names_sep = "_",
               values_to = "value")


########### RM columns i dont need
rm(EM2_df)
