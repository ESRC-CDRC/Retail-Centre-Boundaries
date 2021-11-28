This read-me goes over the broad functions (and ordering) of the code scripts needed for replicating the set of CDRC retail agglomerations. The different pieces of code respectively go through the data cleaning and processing phase, and then through the application of these data sources to the delineation (and refinement) of retail clusters across the whole of the United Kingdom. The code is written in R and makes use primarily of the data.table, sf, h3, osmdata and tidyverse libraries.

No data files are uploaded directly to this repository - rather, a series of blank text files with the corresponding data file names are included in place across the folder structure to show the location and files which are loaded and exported during the delineation process. 

Final data files related to the retail agglomeration boundaries are found at:

https://data.cdrc.ac.uk/dataset/retail-centre-boundaries


The different components of the code include: 

Functions.R
- Set of built functions which have been developed for use during the data cleaning and delineation process.

1_Base_VOA.R
- Script for cleaning the downloaded 2017 VOA ratings data. Merging and cleaning of the multi-files and formats are done with cleaned retail points for England and Wales saved for use. 

1_Base_OSM.R
- Script for downloading and preparing the respective OSM retail points and retail land use data. Data is downloaded and saved separated for England and Wales, Scotland and Northern Ireland. 


** Exports generated from the OSM and VOA (source) data cleaning are stored in the /Data/ subfolders respectively.


2_RC_EngWal.R
- Script to generate the first pass of complete retail agglomerations which will eventually be pruned. Specific code tailored here for England and Wales. 

2_RC_Scotland.R
- Script to generate the first pass of complete retail agglomerations which will eventually be pruned. Specific code tailored here for Scotland. 

2_RC_NorthIreland.R
- Script to generate the first pass of complete retail agglomerations which will eventually be pruned. Specific code tailored here for Northern Ireland. 


** Exports generated from the retail centre delineation are stored in the /Exports/Interim/ subfolder.


3_Manual_Edits.R
- A series of cleaning and pruning are done to the individual H3 units to split, merge, dissolve or delete larger retail agglomerations. The edits are based on bespoke validation of the retail clusters from local stakeholders with knowledge of the area.


4_Naming_Lookup.R
- Different datasets are used to extract conventional names for each of the retail agglomerations. Names are taken from ONS administrative boundaries, OS Open Names dataset, and VOA address information. Extracted lookup tables are saved in the /Exports/Interim/ subfolder.


5_Generate_Exports.R
- The final data product and corresponding (size) statistics are generated. A classification hierarchy is generated for each retail agglomeration based on their underlying retail count and relative ranking within the respective area. Conventional names for each are compiled from the extracted ONS, OS and VOA naming lookups.
