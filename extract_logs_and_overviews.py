'''
This script is made to extract the generated overviews and logs
and write them to the appropriate folders without any alterations
except for file name.
'''

import os
import shutil

#Settings
workdir = "d:/Projects/DairyNZ/Support_FMT/Optimisation/"
#top_folders = ["GAMS","GAMS_FSM","GAMS_EoFM"]
top_folders = ["GAMS_UPDATE_manual_solve"]
sub_folders = ["optimisation_catchment","optimisation_farm","optimisation_nutrientcap"]

#Fuctions
def get_immediate_subdirectories(a_dir):
    return [name for name in os.listdir(a_dir)
            if os.path.isdir(os.path.join(a_dir, name))]

#Loop through folders
for top in top_folders:

	#Check if top folder exists
	if(os.path.isdir(workdir + top) == False):
		raise ValueError("Directory not found : '" + top + "''")
	
	for sub in sub_folders:

		#Check if top folder exists
		if(os.path.isdir(workdir + top + "/" + sub) == False):
			raise ValueError("Directory not found : '" + top + "/" + sub + "''")
	

		#Get scenario folders in subdir
		scen_folders = get_immediate_subdirectories(workdir + top + "/" + sub)

		#Loop scenario folders
		for scen in scen_folders:

			#Copy overview and log
			from_path = workdir + top + "/" + sub + "/" + scen + "/" 
			to_path = workdir + "results_NEW_manual_solve" + "/" + top + "/" + sub + "/" 
			to_path_end = "_" + scen 
			
			from_path_overview = from_path + "overview.csv" 
			to_path_overview = to_path + "overview" + to_path_end + ".csv"
			from_path_log = from_path + "log.csv"
			to_path_log = to_path + "log" + to_path_end + ".csv"
			
			if(os.path.isfile(from_path_overview)):
				shutil.copyfile(from_path_overview,to_path_overview)
				shutil.copyfile(from_path_log,to_path_log)
			else:
				pass




#Finsih up
print("Done.")