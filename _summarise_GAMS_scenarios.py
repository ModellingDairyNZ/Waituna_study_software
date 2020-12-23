'''
Summarise the Scenarios

'''

import pandas as pd
import os
import sys

#Range function
def drange(start, stop, step):
	range_list = []
	r = start
	while r <= stop:
		range_list.append(r)
		r += step

	return(range_list)

#Read arguments
args = sys.argv

#Configuration
#start = 0
#end = 100
#step = 5
#scen_overview = "overview.csv"
#scen_log = "log.csv"
#folder_start = "nitrogen_"
#folder_end = "red"


#Configuration
start = int(args[1])
end = int(args[2])
step = int(args[3])
scen_overview = args[4]
scen_log = args[5]
folder_start = args[6]
folder_end = args[7]

#Create range
range_scen = drange(start,end,step)

#Setup Pandas dataframe to fill
OverviewDF = pd.DataFrame()
LogDF = pd.DataFrame()

progress_log = []

for scen in range_scen:
	
	#Assemble scenario folder
	scen_folder = folder_start + str(scen)+folder_end
	
	#Check if log exists in folder
	path_log = scen_folder +"/" + scen_log
	if(os.path.isfile(path_log)):
		tempLogDF = pd.read_csv(path_log, index_col = False, sep = ";")
		LogDF =  pd.concat([LogDF,tempLogDF.loc[:,['farm','EBIT_K','N_red','P_red','NPrice','PPrice']]])
	else:
		progress_log.append(scen_folder + " : " + scen_log + "not found.")
	#Check if overview exists in folder
	path_overview = scen_folder +"/" + scen_overview
	if(os.path.isfile(path_overview)):
		tempOverviewDF = pd.read_csv(path_overview, sep = ";")
		OverviewDF =  pd.concat([OverviewDF,tempOverviewDF.loc[tempOverviewDF['farm'] == "Total",['EBIT','N_reduction','farmN','NLRedFSM','NLRedEoFM','P_reduction','farmP','PLRedFSM','PLRedEoFM']]])
	else:
		progress_log.append(scen_folder + " : " + scen_overview + "not found.")

#Order dataframes
LogDF = LogDF.drop_duplicates(subset = ['farm','N_red','P_red'], keep = "first")
LogDF['farmNR'] = [int(i.replace('WA','')[0]) for i in LogDF['farm'].tolist()] 
LogDF = LogDF.sort_values(by=['farmNR','N_red','P_red'], ascending = [True,True,True])
OverviewDF = OverviewDF.sort_values(by=['EBIT'], ascending = [False])

#return scenario csvs
LogDF.to_csv("overview_" + folder_start + "scenarios.csv",sep = ";", index = False)
OverviewDF.to_csv("overview_" + folder_start + "scenarios_red.csv",sep = ";", index = False)

#return log
log_file = open("overview_" + folder_start + "log.csv",'w')
log_file.writelines("\n".join(progress_log))
log_file.close()

print("Done.")