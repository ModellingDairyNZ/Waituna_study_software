# Waituna_study_software
Data and software used by the DairyNZ and Deltares team for the Waituna catchment-lagoon optimisation study
In this repo the reader will find the following files:

Example of an input file:
  AreaProf.csv
  
Example of the Optimizer output combined for all farms:
  GAMS_UPDATE
  GAMS_nitrogen_optimisation_nutrientcap_3Nov2020
  
R code for processing results:
  Waituna_data.R
  
Python code for guiding the Optimizer:
  .py files
  
.bat and .ini files for automated running of the Optimizer

Examples of output files with data for individual farms:
  abbreviations for files are:
    catchment = c
    farm = f
    nutrient cap = n
    nitrogen = n
    phosphorous = p
    optimization = o
    
  nutrient cap sensitivity analysis:
    nutrientcap_overview_dataset_usethis_3Nov2020
    
  nutrient losses per farm at zero mitigations (loss for all hectares)
    overview_phosphorous_0red
    overview_nitrogen_0red
    
The GAMS code:
  python_GAMS_all_farms_frame.gms
  
For more information contact pierre.beukes@dairynz.co.nz
