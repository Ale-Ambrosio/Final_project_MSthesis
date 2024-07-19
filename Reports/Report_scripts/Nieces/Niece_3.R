
###########  Packages Loading --------------------------------------------------------

library(skimr)
library(dplyr)
library(tidyr)
library (here)

###########  Reading DF --------------------------------------------------------------

#First we need to concatenate the chunks of data downloaded from Avicenna
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
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that all columns have the same name has failed. This happened at line 39 in the Niece_n.R script.")
}

EM3_df = bind_rows (check_0_25, check_26_50, check_50_100, check_101_129)

rm(check_0_25, check_26_50, check_50_100, check_101_129)

###########  Get rid of metadata -----------------------------------------------------

# Identify columns containing 'metadata' in their names
metadata_cols <- grep("metadata", names(EM3_df), ignore.case = TRUE)


# Remove columns containing 'metadata' both in the original_df and EM3_df
EM3_df <- EM3_df[, -metadata_cols]

# remove metadata variable
rm(metadata_cols)

###########  Change Variable Names ---------------------------------------------------

old_to_new_columns = c(
  
  'Participant.ID' = 'Participant.ID',
  'Participant.Label' = 'Participant.Label',
  'Session.Scheduled.Time' = 'Session.Scheduled.Time',
  
  'EM3_Cracker/beschuit/rijstwafel(met beleg)' = 'X.51_MAQ_1..Cracker.beschuit.rijstwafel..met.beleg.',
  'EM3_(Stok)brood(je)/boterham(met beleg)' = 'X.51_MAQ_2...Stok.brood.je..boterham..met.beleg.',
  'EM3_Zoet.brood(je)/pannenkoeken/poffertjes/wentelteefjes' = 'X.51_MAQ_3..Zoet.brood.je..pannenkoeken.poffertjes.wentelteefj',
  'EM3_Ontbijtpap/havermout/muesli(met/zonder.melk)' = 'X.51_MAQ_4..Ontbijtpap.havermout.muesli..met.zonder.melk.',
  'EM3_Cornflakes/zoete.ontbijtgranen(met/zonder.melk)' = 'X.51_MAQ_5..Cornflakes.zoete.ontbijtgranen..met.zonder.melk.',
  'EM3_Cruesli/granola(met/zonder.melk)' = 'X.51_MAQ_6..Cruesli.granola..met.zonder.melk.',
  
  'EM3_(Plantaardige)yoghurt/kwark' = 'X.52_MAQ_1...Plantaardige..yoghurt.kwark',
  'EM3_Ei' = 'X.52_MAQ_2..Ei',
  
  'EM3_Fruit/fruitsalade' = 'X.53_MAQ_1..Fruit.fruitsalade',
  'EM3_(Snoep/snack)groente' = 'X.53_MAQ_2...Snoep.snack.groente',
  'EM3_Bijgerecht.salade' = 'X.53_MAQ_3..Bijgerecht.salade',
  'EM3_Gedroogde.vruchten' = 'X.53_MAQ_4..Gedroogde.vruchten',
  
  'EM3_Aardappel-groente-vlees/vis/vleesvervanger' = 'X.54_MAQ_1..Aardappel.groente.vlees.vis.vleesvervanger',
  'EM3_Rijst/noedel/pasta-gerecht' = 'X.54_MAQ_2..Rijst.noedel.pasta.gerecht',
  'EM3_Couscous/bulgur/quinoa-gerecht' = 'X.54_MAQ_3..Couscous.bulgur.quinoa.gerecht',
  'EM3_Gevuld broodje/wrap/taco/pita' = 'X.54_MAQ_4..Gevuld.broodje.wrap.taco.pita',
  'EM3_Quiche/hartige.taart/zelfgemaakte.pizza' = 'X.54_MAQ_5..Quiche.hartige.taart.zelfgemaakte.pizza',
  'EM3_Sushi/tapas' = 'X.54_MAQ_6..Sushi.tapas',
  'EM3_Friet/gefrituurde.snack/pizza' = 'X.54_MAQ_7..Friet.gefrituurde.snack.pizza',
  'EM3_Maaltijdsalade' = 'X.54_MAQ_8..Maaltijdsalade',
  
  'EM3_Granenreep/proteïnereep/ontbijtkoek/fruitbiscuit' = 'X.55_MAQ_1..Granenreep.proteïnereep.ontbijtkoek.fruitbiscuit',
  'EM3_Koekje/wafel' = 'X.55_MAQ_2..Koekje.wafel',
  'EM3_Chips/gezouten.toastjes.of.noten/popcorn' = 'X.55_MAQ_3..Chips.gezouten.toastjes.of.noten.popcorn',
  'EM3_Chocolade(reep)/M&Ms/snoep' = 'X.55_MAQ_4..Chocolade.reep..M.Ms.snoep',
  'EM3_Ongezouten.noten/pitten/zaden' = 'X.55_MAQ_5..Ongezouten.noten.pitten.zaden',
  'EM3_Gefrituurde.snack' = 'X.55_MAQ_6..Gefrituurde.snack',
  'EM3_Kaasjes/worstjes' = 'X.55_MAQ_7..Kaasjes.worstjes',
  
  'EM3_Taart/gebak/vlaai/bonbons/soesjes' = 'X.56_MAQ_1..Taart.gebak.vlaai.bonbons.soesjes',
  'EM3_Roomijs/ijstaart' = 'X.56_MAQ_2..Roomijs.ijstaart',
  'EM3_Vla/pudding/dessert' = 'X.56_MAQ_3..Vla.pudding.dessert',
  'EM3_Waterijs/sorbetijs' = 'X.56_MAQ_4..Waterijs.sorbetijs',
  'EM3_Kaasplank' = 'X.56_MAQ_5..Kaasplank',
  
  'EatingPlan' = 'X.57_SAQ..Eetmoment.plan',
  
  'Categ_EMOTIE_Bang/angstig' = 'X.58_MAQ_1..Bang.angstig',
  'Categ_EMOTIE_Geïrriteerd/boos' = 'X.58_MAQ_2..Geïrriteerd.boos',
  'Categ_EMOTIE_Gestrest' = 'X.58_MAQ_3..Gestrest',
  'Categ_EMOTIE_Ontspannen/kalm' = 'X.58_MAQ_4..Ontspannen.kalm',
  'Categ_EMOTIE_Blij/vrolijk' = 'X.58_MAQ_5..Blij.vrolijk',
  'Categ_EMOTIE_Droevig/somber' = 'X.58_MAQ_6..Droevig.somber',
  'Categ_EMOTIE_Verveeld' = 'X.58_MAQ_7..Verveeld',
  'Categ_EMOTIE_Vermoeid' = 'X.58_MAQ_8..Vermoeid',
  'Categ_EMOTIE_Energiek' = 'X.58_MAQ_9..Energiek',
  'Categ_EMOTIE_Zelfverzekerd' = 'X.58_MAQ_10..Zelfverzekerd',
  'Categ_EMOTIE_Onzeker' = 'X.58_MAQ_11..Onzeker',
  'Categ_EMOTIE_Beschaamd/schuldig' = 'X.58_MAQ_12..Beschaamd.schuldig',
  'Categ_EMOTIE_Eenzaam' = 'X.58_MAQ_13..Eenzaam',
  
  'WIE_Niemand' = 'X.59_MAQ_1..Niemand',
  'WIE_Partner' = 'X.59_MAQ_2..Partner',
  'WIE_Gezinsleden/huisgenoten' = 'X.59_MAQ_3..Gezinsleden.huisgenoten',
  'WIE_Collegas/klasgenoten' = 'X.59_MAQ_4..Collega.s.klasgenoten',
  'WIE_Onbekenden' = 'X.59_MAQ_5..Onbekenden',
  'WIE_Anders' = 'X.59_MAQ_6..Anders',
  'WIE_Vrienden' = 'X.59_MAQ_7..Vrienden',
  'WIE_Overige.familieleden' = 'X.59_MAQ_8..Overige.familieleden',
  
  'EatmomentWaar' = 'X.60_SAQ..Eetmoment..waar',
  
  'ACTIVITY_Nee' = 'X.61_MAQ_1..Nee',
  'ACTIVITY_Lezen(boek,krant,nieuws)' = 'X.61_MAQ_2..Lezen..boek.krant.nieuws.',
  'ACTIVITY_Scrollen.op.social.media' = 'X.61_MAQ_3..Scrollen.op.social.media',
  'ACTIVITY_TV.kijken' = 'X.61_MAQ_4..TV.kijken',
  'ACTIVITY_Spelletje.spelen' = 'X.61_MAQ_5..Spelletje.spelen',
  'ACTIVITY_Werken/studeren' = 'X.61_MAQ_6..Werken.studeren',
  'ACTIVITY_Praten' = 'X.61_MAQ_7..Praten',
  'ACTIVITY_Anders' = 'X.61_MAQ_8..Anders',
  'ACTIVITY_Koken' = 'X.61_MAQ_9..Koken',
  
  'EM3_Soep' = 'X.181_MAQ_1..Soep',
  'EM3_Gevulde.soep/maaltijdsoep' = 'X.181_MAQ_2..Gevulde.soep.maaltijdsoep',
  
  ## Addition tijd 19 - March !
  'EatmomentTijd' = 'X.111_SAQ..Eetmoment.tijd',
  
  'Intensity_EMOTIE_Bang' = 'X.130_VAS..Eetmoment.Intensiteit.bang',
  'Intensity_EMOTIE_Geirriteerd' = 'X.133_VAS..Eetmoment.intensiteit.geïrriteerd',
  'Intensity_EMOTIE_Gestrest' = 'X.136_VAS..Eetmoment.intensiteit.gestrest',
  'Intensity_EMOTIE_Kalm' = 'X.140_VAS..Eetmoment.intensiteit.ontspannen',
  'Intensity_EMOTIE_Blij' = 'X.144_VAS..Eetmoment.intensiteit.blij',
  'Intensity_EMOTIE_Droevig' = 'X.148_VAS..Eetmoment.intensiteit.droevig',
  'Intensity_EMOTIE_Verveeld' = 'X.152_VAS..Eetmoment.intensiteit.verveeld',
  'Intensity_EMOTIE_Vermoeid' = 'X.156_VAS..Eetmoment.intensiteit.vermoeid',
  'Intensity_EMOTIE_Energiek' = 'X.160_VAS..Eetmoment.intensiteit.energiek',
  'Intensity_EMOTIE_Zelfverzekerd' = 'X.164_VAS..Eetmoment.intensiteit.zelfverzekerd',
  'Intensity_EMOTIE_Onzeker' = 'X.168_VAS..Eetmoment.intensiteit.onzeker',
  'Intensity_EMOTIE_Beschaamd' = 'X.172_VAS..Eetmoment.intensiteit.beschaamd',
  'Intensity_EMOTIE_Eenzam' = 'X.176_VAS..Eetmoment.intensiteit.eenzaam')


# Now renaming

EM3_df = EM3_df %>% rename(old_to_new_columns)

# Now cutting EM3_df to only the renamed columns above

# Extract the renamed column names
renamed_columns <- names(old_to_new_columns)

# Subset the mtcars data frame to keep only the renamed columns
EM3_df <- EM3_df %>% select(any_of((renamed_columns)))

rm(renamed_columns, old_to_new_columns)

########### Checking for Duplicates -------------------------------------------------

#Checking for duplicated rows
duplicates <- duplicated(EM3_df)

#we remove them
EM3_df=EM3_df[!duplicates,]

# sanity check to make sure all variable names are unique

if (any(duplicated(names(EM3_df))) != FALSE) {
  stop("ERROR! Duplicated column names found in the third EM data frame! This happened at line 187, Script Niece_3.R.")
}

###########  Make long format by EM_ (food categories) -------------------------------

EM3_df_long <- EM3_df %>%
  pivot_longer(cols = starts_with("EM"),
               names_to = c("EatingMomentNumber", "FoodSubcategory"),
               names_sep = "_",
               values_to = "value")


###########  RM columns i dont need
rm(EM3_df)
