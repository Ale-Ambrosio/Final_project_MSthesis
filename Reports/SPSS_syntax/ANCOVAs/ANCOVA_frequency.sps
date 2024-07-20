* Encoding: UTF-8.

PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="C:\Users\ambro\Downloads\Network "+
    "coding\Final_project_MSthesis\Reports\Report_output\Dataframes\Whole_sample\For_ANOVA_frequenc"+
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
  Freq_Stressed AUTO
  Freq_Sad AUTO
  Freq_Happy AUTO
  Freq_Bored AUTO
  Group AUTO
  EE_score AUTO
  /MAP.
RESTORE.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.

/*
Stop running here and check the format of the variables. 
["Freq_Stressed", "Freq_Sad", "Freq_Happy", "Freq_Bored"] need to be Scale formatted.
You can modify it manually in the Variable view.
*/


DATASET ACTIVATE DataSet1.
COMPUTE EE_centered=EE_score - 36.36.
EXECUTE.

UNIANOVA Freq_Stressed BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

UNIANOVA Freq_Sad BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

UNIANOVA Freq_Happy BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.

UNIANOVA Freq_Bored BY Label WITH EE_centered
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PLOT=PROFILE(Label) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /EMMEANS=TABLES(Label) WITH(EE_centered=MEAN) COMPARE ADJ(LSD)
  /PRINT DESCRIPTIVE HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN=EE_centered*Label Label EE_centered.
