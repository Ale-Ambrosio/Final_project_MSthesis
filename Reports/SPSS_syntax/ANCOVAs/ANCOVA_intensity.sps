* Encoding: UTF-8.

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="C:\Users\ambro\Downloads\Network "+
    "coding\Final_project_MSthesis\Reports\Report_output\Dataframes\Whole_sample\For_ANOVA_intensit"+
    "y_whole.csv"
  /ENCODING='UTF8'
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  Participant.Label AUTO
  Label AUTO
  Int_Stressed AUTO
  Int_Sad AUTO
  Int_Happy AUTO
  Int_Bored AUTO
  Group AUTO
  EE_score AUTO
  /MAP.
RESTORE.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.

/*
Stop running here and check the format of the variables. 
["Int_Stressed", "Int_Sad", "Int_Happy", "Int_Bored"] need to be Scale formatted.
You can modify it manually in the Variable view.
*/


DATASET ACTIVATE DataSet1.
COMPUTE EE_centered=EE_score - 36.36.
EXECUTE.



UNIANOVA Int_Stressed BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

UNIANOVA Int_Sad BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

UNIANOVA Int_Happy BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

UNIANOVA Int_Bored BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

