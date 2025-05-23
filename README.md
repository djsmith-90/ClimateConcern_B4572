## Git repository for 'Climate concern and mental health/well-being' ALSPAC project (B4572)

The main directory in this repository contains seven R scripts for the actual analyses. These files are:
 - Script1_DataProcessing.R - Script to clean and process the raw ALSPAC data
 - Script2_SyntheticData.R - Script to create synthetic datasets using the 'synthpop' package
 - Script3_MainEffectsAnalyses.R - Script to analyse the data for the main effect analyses
 - Script4a_Moderators_Depression.R - Script to analyse the data for the effect modification analyses with depression as the outcome
 - Script4b_Moderators_Anxiety.R - Script to analyse the data for the effect modification analyses with anxiety as the outcome
 - Script4c_Moderators_Wellbeing.R - Script to analyse the data for the effect modification analyses with well-being as the outcome
 - Script5_Plots.R - Script to make plots of the results
 
 
The 'SyntheticData' folder also contains synthetic versions of the ALSPAC dataset, created
using Script 2 above. As raw ALSPAC data cannot be released, these synthesised datasets are modelled on the original 
ALSPAC data, thus maintaining variable distributions and relations among variables (albeit not pefectly), while 
at the same time preserving participant anonymity and confidentiality. Please note that while these synthetic datasets 
can be used to follow the analysis scripts, as data are simulated they should *not* be used for research purposes; 
only the actual, observed, ALSPAC data should be used for formal research and analyses reported in published work.

These synthetic datasets have the file name 'syntheticData_climateConcern_B4572' and are available in R ('.RData'),
Data ('.dta') and CSV ('.csv') formats. A codebook describing the data file is also in this folder ('climateConcern_codebook.xlsx')


The 'forPrereg' folder contains scripts created as part of the Stage I Registered Report, demonstrating the proposed 
analyses using simulated data ('ClimateConcernAndMH_ExampleAnalysisCode.r') and for power analyses 
('ClimateConcernAndMH_PowerAnalysis.r').


Note that ALSPAC data access is through a system of managed open access. Information about access to ALSPAC data is 
given on the ALSPAC website (http://www.bristol.ac.uk/alspac/researchers/access/). The datasets used in these
scripts are linked to ALSPAC project number B4572; if you are interested in accessing these datasets, please quote 
this number during your application.
