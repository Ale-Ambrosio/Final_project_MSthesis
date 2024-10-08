# Final_project_MSthesis

## Table of Contents
- [Emotional eating scores calculation](#Emotional-eating-scores-and-groups)
- [Network Estimation](#Network-estimation)
- [Networks Comparison](#Networks-comparison)
- [Obtaining the data frames for the ANOVA](#Data-processing-for-ANOVAs)
- [SPSS analyses](#SPSS-analyses)

## Emotional eating scores and groups 
1. Go to the Qualtrics questionnaire "EoA - Baseline Measures" and Export the data, selecting the option ```Use numerical values```. Move the file in the folder ```EE_calculation\Dataframes\Input_data``` and call it ```Baseline_measures```. A file called ```Qualtrics_ID``` should already be present in the same folder.
2. Open the file ```Final_project_MSthesis.Rproj```.
3. Open the script ```For_EE_dataset```, which is placed in the folder ```EE_calculation\Scripts```. Run it pressing on ```Source```.
4. The output will be in the folders ```EE_calculation\Graphs``` (where you will find a boxplot and a density plot) and ```EE_calculation\Dataframes\Output_data``` (where you will find two ```.docx``` for the codes lists and two ```.csv``` for the two dataframes).


## Network estimation
1. Download the GitHub folder ```Final_project_MSthesis``` on your laptop
2. Download the lifestyle data from Avicenna using filters (for Participant Label) to obtain these chunks: a) EoA001-EoA025, b) EoA0026 - EoA049, c)EoA051 - EoA100, d)EoA101 - EoA129. Save them on your laptop respectively with these names: a) ```lifestyle_all_0-25.csv```, b) ```lifestyle_all_26-50.csv```, c) ```lifestyle_all_50-100.csv```, d) ```lifestyle_all_101-129.csv```. The files should be placed in the folder ```Networks\Network_data\Input_data\All_Participants```.
3. Make sure you don't have any files in the folders ```Networks\Network_data\Input_data\All_Participants\Filtered_data_whole``` and ```Networks\Network_data\Input_data\All_Participants\Filtered_data_extremes```
4. Open the file ```Final_project_MSthesis.Rproj```.
5. Open the script ```Main.networks.R``` which is placed in the folder ```Networks\Network_scripts```. Run it pressing on ```Source```.
6. In the folder ```Networks\Network_output\Estimation``` you will find the pictures of your networks and a document with all the references of the R session. Below there is an example of how the networks should look like. 

![](Example_figures/contemporaneous_example.jpg)

## Networks comparison
1. Run this script only after performing the previous step (```Network estimation```), and don't modify the output.
2. Open the file ```Final_project_MSthesis.Rproj```.
3. Open the script ```Networks_comparison.R``` in the folder ```Networks\Network_scripts```. Run it using ```Source```.
4. In the folder ```Networks\Network_output\Comparison``` you will find the PDFs and matrices of your comparisons and a document with all the references of the R session. Below there is an example of how the output should look like

![](Example_figures/comparison_example.jpg)


## Data processing for ANOVAs
1. Download the lifestyle data from Avicenna using filters (for Participant Label) to obtain these chunks: a) EoA001-EoA025, b) EoA0026 - EoA049, c)EoA051 - EoA100, d)EoA101 - EoA129. Save them on your laptop respectively with these names: a) ```lifestyle_all_0-25.csv```, b) ```lifestyle_all_26-50.csv```, c) ```lifestyle_all_50-100.csv```, d) ```lifestyle_all_101-129.csv```. The files should be placed in the folder ```Reports\Report_data\Input_data```.
2. Open the file ```Final_project_MSthesis.Rproj```.
3. Open the script ```Report_script.R``` which is placed in the folder ```Reports\Report_scripts```. Run it pressing on ```Source```.
4. In the folder ```Reports\Report_output\Dataframes``` you will find the dataframes to read in SPSS.
5. If you would like to produce the graphics regarding eating moments, emotions and EE groups, run the script ```Report_graphs.R```, which is placed in the folder ```Reports\Report_scripts```. Results will be stored in the folder ```Reports\Report_output\Graphs```. You should run this script only after having performed the previous step (```Report_script.R```).

## SPSS analyses 
1. The dataframes obtained from the previous step (```Data processing for ANOVAs```) can be processed running the syntax files that you find in the folder ```Reports\SPSS_syntax```.
2. When you open the SPSS syntax doc (```.sps```), make sure to properly modify the path for the data import. At line 7 you will find the following lines (see below). Adjust the portion of the path that is before ```Final_project_MSthesis``` according to the location where you placed the GitHub folder in your laptop.

```diff
/FILE="C:\Users\ambro\Downloads\Network "+
    "coding\Final_project_MSthesis\Reports\Report_output\Dataframes\Extremes_groups\For_ANOVA_inten"+
    "sity_extremes.csv"
```

3. Don't run the whole syntax at the beginning as there is one check to do after data import. In the syntax you will find the following lines (see below). Run the syntax until this point, perform the indicated check, and then you can run the rest of the syntax in one time. 
```diff
Stop running here and check the format of the variables. 
["Freq_Stressed", "Freq_Sad", "Freq_Happy", "Freq_Bored"] need to be Scale formatted.
You can modify it manually in the Variable view.
```
4. If you wish, you can save your output in a ```.pdf``` or ```.spv``` directly throught the SPSS Output window.
