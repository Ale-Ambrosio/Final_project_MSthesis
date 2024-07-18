# Final_project_MSthesis
 
## Network estimation
1. Download the GitHub folder ```Final_project_MSthesis``` on your laptop
2. Download the lifestyle data from Avicenna using filters (for Participant Label) to obtain these chunks: a) EoA001-EoA025, b) EoA0026 - EoA050, c)EoA051 - EoA100, d)EoA101 - EoA129. Save them on your laptop respectively with these names: a) ```lifestyle_all_0-25.csv```, b) ```lifestyle_all_26-50.csv```, c) ```lifestyle_all_50-100.csv```, d) ```lifestyle_all_101-129.csv```. The files should be placed in the folder ```Networks\Network_data\Input_data\All_Participants```.
3. Make sure you don't have any files in the folders ```Networks\Network_data\Input_data\All_Participants\Filtered_data_whole``` and ```Networks\Network_data\Input_data\All_Participants\Filtered_data_extremes```
4. Open the file ```Final_project_MSthesis.Rproj```.
5. Open the script ```Main.networks.R``` which is placed in the folder ```Networks\Network_scripts```. Run it pressing on ```Source```.
6. In the folder ```Networks\Network_output\Estimation``` you will find the pictures of your networks and a document with all the references of the R session

## Networks comparison
1. Run this script only after performing the previous step (```Network estimation```), and don't modify the output.
2. Open the file ```Final_project_MSthesis.Rproj```.
3. Open the script ```Networks_comparison.R``` in the folder ```Networks\Network_scripts```. Run it using ```Source```.
4. In the folder ```Networks\Network_output\Comparison``` you will find the PDFs and matrices of your comparisons and a document with all the references of the R session
