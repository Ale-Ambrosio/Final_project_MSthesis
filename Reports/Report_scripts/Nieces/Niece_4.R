#to pre-process EM_4

########### Packages Loading --------------------------------------------------------

library(skimr)
library(dplyr)
library(tidyr)
library(here)

########### Reading DF --------------------------------------------------------------

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
  stop("ERROR! When concat vertically the chunked files from Avicenna, the check that all columns have the same name has failed. This happened at line 37 in the Niece_n.R script.")
}

EM4_df = bind_rows (check_0_25, check_26_50, check_50_100, check_101_129)

rm(check_0_25, check_26_50, check_50_100, check_101_129)


########### Get rid of metadata -----------------------------------------------------

# Identify columns containing 'metadata' in their names
metadata_cols <- grep("metadata", names(EM4_df), ignore.case = TRUE)


# Remove columns containing 'metadata' both in the original_df and EM4_df
EM4_df <- EM4_df[, -metadata_cols]


# remove metadata variable
rm(metadata_cols)


########### Change Variable Names ---------------------------------------------------

old_to_new_columns = c(
  
  'Participant.ID' = 'Participant.ID',
  'Participant.Label' = 'Participant.Label',
  'Session.Scheduled.Time' = 'Session.Scheduled.Time',
  
  'EM4_Cracker/beschuit/rijstwafel(met beleg)' = 'X.64_MAQ_1..Cracker.beschuit.rijstwafel..met.beleg.',
  'EM4_(Stok)brood(je)/boterham(met beleg)' = 'X.64_MAQ_2...Stok.brood.je..boterham..met.beleg.',
  'EM4_Zoet.brood(je)/pannenkoeken/poffertjes/wentelteefjes' = 'X.64_MAQ_3..Zoet.brood.je..pannenkoeken.poffertjes.wentelteefj',
  'EM4_Ontbijtpap/havermout/muesli(met/zonder.melk)' = 'X.64_MAQ_4..Ontbijtpap.havermout.muesli..met.zonder.melk.',
  'EM4_Cornflakes/zoete.ontbijtgranen(met/zonder.melk)' = 'X.64_MAQ_5..Cornflakes.zoete.ontbijtgranen..met.zonder.melk.',
  'EM4_Cruesli/granola(met/zonder.melk)' = 'X.64_MAQ_6..Cruesli.granola..met.zonder.melk.',
  
  'EM4_(Plantaardige)yoghurt/kwark' = 'X.65_MAQ_1...Plantaardige..yoghurt.kwark',
  'EM4_Ei' = 'X.65_MAQ_2..Ei',
  
  'EM4_Fruit/fruitsalade' = 'X.66_MAQ_1..Fruit.fruitsalade',
  'EM4_(Snoep/snack)groente' = 'X.66_MAQ_2...Snoep.snack.groente',
  'EM4_Bijgerecht.salade' = 'X.66_MAQ_3..Bijgerecht.salade',
  'EM4_Gedroogde.vruchten' = 'X.66_MAQ_4..Gedroogde.vruchten',
  
  'EM4_Aardappel-groente-vlees/vis/vleesvervanger' = 'X.67_MAQ_1..Aardappel.groente.vlees.vis.vleesvervanger',
  'EM4_Rijst/noedel/pasta-gerecht' = 'X.67_MAQ_2..Rijst.noedel.pasta.gerecht',
  'EM4_Couscous/bulgur/quinoa-gerecht' = 'X.67_MAQ_3..Couscous.bulgur.quinoa.gerecht',
  'EM4_Gevuld broodje/wrap/taco/pita' = 'X.67_MAQ_4..Gevuld.broodje.wrap.taco.pita',
  'EM4_Quiche/hartige.taart/zelfgemaakte.pizza' = 'X.67_MAQ_5..Quiche.hartige.taart.zelfgemaakte.pizza',
  'EM4_Sushi/tapas' = 'X.67_MAQ_6..Sushi.tapas',
  'EM4_Friet/gefrituurde.snack/pizza' = 'X.67_MAQ_7..Friet.gefrituurde.snack.pizza',
  'EM4_Maaltijdsalade' = 'X.67_MAQ_8..Maaltijdsalade',
  
  'EM4_Granenreep/proteïnereep/ontbijtkoek/fruitbiscuit' = 'X.68_MAQ_1..Granenreep.proteïnereep.ontbijtkoek.fruitbiscuit',
  'EM4_Koekje/wafel' = 'X.68_MAQ_2..Koekje.wafel',
  'EM4_Chips/gezouten.toastjes.of.noten/popcorn' = 'X.68_MAQ_3..Chips.gezouten.toastjes.of.noten.popcorn',
  'EM4_Chocolade(reep)/M&Ms/snoep' = 'X.68_MAQ_4..Chocolade.reep..M.Ms.snoep',
  'EM4_Ongezouten.noten/pitten/zaden' = 'X.68_MAQ_5..Ongezouten.noten.pitten.zaden',
  'EM4_Gefrituurde.snack' = 'X.68_MAQ_6..Gefrituurde.snack',
  'EM4_Kaasjes/worstjes' = 'X.68_MAQ_7..Kaasjes.worstjes',
  
  'EM4_Taart/gebak/vlaai/bonbons/soesjes' = 'X.69_MAQ_1..Taart.gebak.vlaai.bonbons.soesjes',
  'EM4_Roomijs/ijstaart' = 'X.69_MAQ_2..Roomijs.ijstaart',
  'EM4_Vla/pudding/dessert' = 'X.69_MAQ_3..Vla.pudding.dessert',
  'EM4_Waterijs/sorbetijs' = 'X.69_MAQ_4..Waterijs.sorbetijs',
  'EM4_Kaasplank' = 'X.69_MAQ_5..Kaasplank',
  
  'EatingPlan' = 'X.70_SAQ..Eetmoment.plan',
  
  'Categ_EMOTIE_Bang/angstig' = 'X.71_MAQ_1..Bang.angstig',
  'Categ_EMOTIE_Geïrriteerd/boos' = 'X.71_MAQ_2..Geïrriteerd.boos',
  'Categ_EMOTIE_Gestrest' = 'X.71_MAQ_3..Gestrest',
  'Categ_EMOTIE_Ontspannen/kalm' = 'X.71_MAQ_4..Ontspannen.kalm',
  'Categ_EMOTIE_Blij/vrolijk' = 'X.71_MAQ_5..Blij.vrolijk',
  'Categ_EMOTIE_Droevig/somber' = 'X.71_MAQ_6..Droevig.somber',
  'Categ_EMOTIE_Verveeld' = 'X.71_MAQ_7..Verveeld',
  'Categ_EMOTIE_Vermoeid' = 'X.71_MAQ_8..Vermoeid',
  'Categ_EMOTIE_Energiek' = 'X.71_MAQ_9..Energiek',
  'Categ_EMOTIE_Zelfverzekerd' = 'X.71_MAQ_10..Zelfverzekerd',
  'Categ_EMOTIE_Onzeker' = 'X.71_MAQ_11..Onzeker',
  'Categ_EMOTIE_Beschaamd/schuldig' = 'X.71_MAQ_12..Beschaamd.schuldig',
  'Categ_EMOTIE_Eenzaam' = 'X.71_MAQ_13..Eenzaam',
  
  'WIE_Niemand' = 'X.72_MAQ_1..Niemand',
  'WIE_Partner' = 'X.72_MAQ_2..Partner',
  'WIE_Gezinsleden/huisgenoten' = 'X.72_MAQ_3..Gezinsleden.huisgenoten',
  'WIE_Collegas/klasgenoten' = 'X.72_MAQ_4..Collega.s.klasgenoten',
  'WIE_Onbekenden' = 'X.72_MAQ_5..Onbekenden',
  'WIE_Anders' = 'X.72_MAQ_6..Anders',
  'WIE_Vrienden' = 'X.72_MAQ_7..Vrienden',
  'WIE_Overige.familieleden' = 'X.72_MAQ_8..Overige.familieleden',
  
  'EatmomentWaar' = 'X.73_SAQ..Eetmoment..waar',
  
  'ACTIVITY_Nee' = 'X.74_MAQ_1..Nee',
  'ACTIVITY_Lezen(boek,krant,nieuws)' = 'X.74_MAQ_2..Lezen..boek.krant.nieuws.',
  'ACTIVITY_Scrollen.op.social.media' = 'X.74_MAQ_3..Scrollen.op.social.media',
  'ACTIVITY_TV.kijken' = 'X.74_MAQ_4..TV.kijken',
  'ACTIVITY_Spelletje.spelen' = 'X.74_MAQ_5..Spelletje.spelen',
  'ACTIVITY_Werken/studeren' = 'X.74_MAQ_6..Werken.studeren',
  'ACTIVITY_Praten' = 'X.74_MAQ_7..Praten',
  'ACTIVITY_Anders' = 'X.74_MAQ_8..Anders',
  'ACTIVITY_Koken' = 'X.74_MAQ_9..Koken',
  
  'EM4_Soep' = 'X.182_MAQ_1..Soep',
  'EM4_Gevulde.soep/maaltijdsoep' = 'X.182_MAQ_2..Gevulde.soep.maaltijdsoep',
  ## Addition tijd 19 - March !
  'EatmomentTijd' = 'X.112_SAQ..Eetmoment.tijd',
  
  'Intensity_EMOTIE_Bang' = 'X.131_VAS..Eetmoment.Intensiteit.bang',
  'Intensity_EMOTIE_Geirriteerd' = 'X.134_VAS..Eetmoment.intensiteit.geïrriteerd',
  'Intensity_EMOTIE_Gestrest' = 'X.137_VAS..Eetmoment.intensiteit.gestrest',
  'Intensity_EMOTIE_Kalm' = 'X.141_VAS..Eetmoment.intensiteit.ontspannen',
  'Intensity_EMOTIE_Blij' = 'X.145_VAS..Eetmoment.intensiteit.blij',
  'Intensity_EMOTIE_Droevig' = 'X.149_VAS..Eetmoment.intensiteit.droevig',
  'Intensity_EMOTIE_Verveeld' = 'X.153_VAS..Eetmoment.intensiteit.verveeld',
  'Intensity_EMOTIE_Vermoeid' = 'X.157_VAS..Eetmoment.intensiteit.vermoeid',
  'Intensity_EMOTIE_Energiek' = 'X.161_VAS..Eetmoment.intensiteit.energiek',
  'Intensity_EMOTIE_Zelfverzekerd' = 'X.165_VAS..Eetmoment.intensiteit.zelfverzekerd',
  'Intensity_EMOTIE_Onzeker' = 'X.169_VAS..Eetmoment.intensiteit.onzeker',
  'Intensity_EMOTIE_Beschaamd' = 'X.173_VAS..Eetmoment.intensiteit.beschaamd',
  'Intensity_EMOTIE_Eenzam' = 'X.177_VAS..Eetmoment.intensiteit.eenzaam')


# now renaming

EM4_df = EM4_df %>% rename(old_to_new_columns)

# Now cutting EM4_df to only the renamed columns above

# Extract the renamed column names
renamed_columns <- names(old_to_new_columns)

# Subset the mtcars data frame to keep only the renamed columns
EM4_df <- EM4_df %>% select(any_of((renamed_columns)))

rm(renamed_columns, old_to_new_columns)

########### Checking for Duplicates -------------------------------------------------

#Checking for duplicated rows
duplicates <- duplicated(EM4_df)

#we remove them
EM4_df=EM4_df[!duplicates,]

# sanity check to make sure all variable names are unique

if (any(duplicated(names(EM4_df))) != FALSE) {
  stop("ERROR! Duplicated column names found in the fourth EM data frame! This happened at line 189, Script Niece_4.R.")
}

########### Make long format by EM_ (food categories) -------------------------------

EM4_df_long <- EM4_df %>%
  pivot_longer(cols = starts_with("EM"),
               names_to = c("EatingMomentNumber", "FoodSubcategory"),
               names_sep = "_",
               values_to = "value")

#rm stuff i dont need
rm(EM4_df)

