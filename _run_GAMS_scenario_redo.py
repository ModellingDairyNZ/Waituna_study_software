#!/usr/bin/python2.7.11

'''
Run GAMS scenario



'''

import os
import sys
import time
import decimal
import shutil

from gams import *

def stringtobool(string):
	if(string.lower() == "true"):
		output = True
	elif(string.lower() == "false"):
		output = False
	else:
		output = None
	return(output)

def readsetup(path):
	'''
	Read the setup of the module
	'''

	setup = open(path,"r").readlines()
	
	#Remove comments
	setup_clean = []
	for line in setup:
		if(not(line.startswith("#"))):
			setup_clean.append(line)
	
	#Add model configuration values
	setup_dict = {}
	for setfk in setup_clean:
		new_key_setup = setfk.rstrip('\n').split("=")
		try:
			setup_dict[new_key_setup[0]] = new_key_setup[1] 
		except:
			raise ValueError("Following setup input not accepted : " + setfk)

	return(setup_dict)


def RunGAMS(gams_dir,gams_scenario,gams_frame_path,gams_farm_path,config_dict,config_solver_file,license_file):
	
	#create file
	gams_file_framework = open(gams_dir + gams_frame_path,'r')
	gams_file_framework_text = gams_file_framework.read()
	new_gams_file_text = gams_file_framework_text.format(**config_dict)
	new_gams_file = open(gams_dir + gams_scenario + gams_farm_path,'w')
	new_gams_file.write(new_gams_file_text)
	new_gams_file.close()
	gams_file_framework.close()

	#Set GAMS Workspace
	ws = GamsWorkspace(working_directory = gams_dir + gams_scenario)

	 #Add GAMS options
	opt = ws.add_options()

	#Set solver
	sol_file = open(config_solver_file,'r')
	sol_file_read = sol_file.read()
	sol_file.close()
	solver_setup = [sol.split("=") for sol in sol_file_read.split("\n")]
	sol_dict = {}
	for solver in solver_setup:
		sol_dict[solver[0]] = solver[1]

	solver_used = sol_dict["MINLP"]
	opt.all_model_types = solver_used

	 
	#Set license
	lic_file = open(license_file,'r')
	lic_file_read = lic_file.read()
	file = open(os.path.join(ws.working_directory,"license.txt"),"w")
	file.write(lic_file_read)
	file.close()
	opt.set_license = 1	

	#Calculate with GAMS
	t1 = ws.add_job_from_file(gams_farm_path)
	t1.run(opt)
	t1.out_db.export(os.path.join(ws.working_directory, gams_farm_path.replace(".gms",".gdx")))

	return(t1)


def PreformModel(iterations, log, NPrice, PPrice, gams_dir, gams_scenario, gams_frame_path):

	#Storage
	overview = []
	overview.append(["farm","z","farmN","NLRedFSM","NLRedEoFM","farmP","PLRedFSM","PLRedEoFM","SOLVER"])

	for i in iterations:


		print(i)

		#CREATE FAMR GAMS FILE

		#Variable Configuration:
		gams_farm_path = "python_GAMS_" + i + ".gms"

		#SET SOLVER
		solvers_setup = []
		solvers_setup.append("Gams_optimisation_COUENNE.opt")
		solvers_setup.append("Gams_optimisation_LINDO.opt")
		license_setup = "..\\license.txt"

		#fill dictonary
		config_dict = {}
		config_dict['farmid'] = i
		config_dict['nprice'] = "{0:.3f}".format(NPrice)
		config_dict['pprice'] = "{0:.3f}".format(PPrice)

		#Run GAMS
		try_output = 0			
		t1 = 0
		used_solver = solvers_setup[0]
		t1 = RunGAMS(gams_dir,gams_scenario,gams_frame_path,gams_farm_path,config_dict,solvers_setup[0],license_setup)

		#Try solvers
		for solver in solvers_setup:  

			if(solver != solvers_setup[0]):
				#See if solution worked else alter slightly
				try:
					farmN 		= [rec.level for rec in t1.out_db["farmN"]][0]
					check	    = farmN/farmN
				except:
					#Run GAMS
					try_output = 0			
					t1 = 0
					used_solver = solver
	
					#fill dictonary
					config_dict['nprice'] = "{0:.3f}".format(NPrice)
					config_dict['pprice'] = "{0:.3f}".format(PPrice)
	
					#Run GAMS
					t1 = RunGAMS(gams_dir,gams_scenario,gams_frame_path,gams_farm_path,config_dict,solver,license_setup)
	

			#See if solution worked else alter slightly
			try:
				farmN 		= [rec.level for rec in t1.out_db["farmN"]][0]
				check	    = farmN/farmN
			except:
				t1=0
				try_output = 1
				used_solver = solver

				#DO TO THE WAY GAMS SOLVES WITH COUENNE A SMALL SHIFT IN PRICE
				# CAN LEAD TO A SOLVABLE SITUATION

				#fill dictonary
				config_dict['nprice'] = "{0:.3f}".format(NPrice + 0.001)
				config_dict['pprice'] = "{0:.3f}".format(PPrice + 0.001)

				#Run GAMS
				t1 = RunGAMS(gams_dir,gams_scenario,gams_frame_path,gams_farm_path, config_dict, solver,license_setup)

			#See if solution worked else alter slightly
			try:
				farmN 		= [rec.level for rec in t1.out_db["farmN"]][0]
				check	    = farmN/farmN
			except:
				t1=0
				try_output = 2
				used_solver = solver

				#DO TO THE WAY GAMS SOLVES WITH COUENNE A SMALL SHIFT IN PRICE
				# CAN LEAD TO A SOLVABLE SITUATION

				#fill dictonary
				config_dict['npric'] = "{0:.3f}".format(NPrice + 0.001)
				config_dict['pprice'] = "{0:.3f}".format(PPrice) 

				#Run GAMS
				t1 = RunGAMS(gams_dir,gams_scenario,gams_frame_path,gams_farm_path, config_dict,solver,license_setup)

			#See if solution worked else alter slightly
			try:
				farmN 		= [rec.level for rec in t1.out_db["farmN"]][0]
				check	    = farmN/farmN
			except:
				t1=0
				try_output = 3
				used_solver = solver

				#DO TO THE WAY GAMS SOLVES WITH COUENNE A SMALL SHIFT IN PRICE
				# CAN LEAD TO A SOLVABLE SITUATION

				#fill dictonary
				config_dict['nprice'] = "{0:.3f}".format(NPrice)
				config_dict['pprice'] = "{0:.3f}".format(PPrice + 0.001)

				#Run GAMS
				t1 = RunGAMS(gams_dir,gams_scenario,gams_frame_path,gams_farm_path, config_dict, solver,license_setup)

		#Write results
		z 		= [rec.level for rec in t1.out_db["zact"]][0]
		farmN 		= [rec.level for rec in t1.out_db["farmN"]][0]
		NLRedFSM 	= [rec.level for rec in t1.out_db["NLRedFSM"]][0]
		NLRedEoFM 	= [rec.level for rec in t1.out_db["NLRedEoFM"]][0]
		farmP 		= [rec.level for rec in t1.out_db["farmP"]][0]
		PLRedFSM 	= [rec.level for rec in t1.out_db["PLRedFSM"]][0]
		PLRedEoFM 	= [rec.level for rec in t1.out_db["PLRedEoFM"]][0]

		overview.append([i,z,farmN,NLRedFSM,NLRedEoFM,farmP,PLRedFSM,PLRedEoFM,used_solver])

	return((overview, log))


def PreformSolve(gams_dict, gams_dir, gams_scenario, farms_path, gams_frame_path):

	farms = open(gams_dir + farms_path,'r')
	farms_txt = farms.read()
	iterations = farms_txt.split("\n")[1:]
	farms.close()

	#Set contraints on model
	Nconstraint = stringtobool(gams_dict["Nconstraint"])
	Pconstraint = stringtobool(gams_dict["Pconstraint"])

	#Set initial price range
	NPrice_min = float(gams_dict["NPrice_min"])
	NPrice_max = float(gams_dict["NPrice_max"])
	PPrice_min = float(gams_dict["PPrice_min"])
	PPrice_max = float(gams_dict["PPrice_max"])

	#Set initial load range
	NLoad = float(gams_dict["NLoad"])
	NLoad_low = float(gams_dict["NLoad_low"])
	NLoad_high = float(gams_dict["NLoad_high"])
	PLoad = float(gams_dict["PLoad"])
	PLoad_low = float(gams_dict["PLoad_low"])
	PLoad_high = float(gams_dict["PLoad_high"])

	#Set maximum of solve loops
	max_solve_loops = int(gams_dict["max_solve_loops"])

	#ADMINISTRATION

	time_start = time.time()
	time_previous = time_start

	#Create log
	log = open(gams_dir + gams_scenario + "log.csv",'w')
	log.write(";".join(["Solve_loop","EBIT_K","Nload_ton","Pload_ton",\
		"NPrice","NPrice_min","NPrice_max","PPrice","PPrice_min",\
		"PPrice_max","time_loop","time_passed"]) +"\n")

	#Add GAMS options
	options_file1 = "Gams_optimisation_COUENNE.opt"
	shutil.copyfile(gams_dir + options_file1, gams_dir + gams_scenario + options_file1)
	options_file2 = "Gams_optimisation_BONMIN.opt"
	shutil.copyfile(gams_dir + options_file2, gams_dir + gams_scenario + options_file2)

	solve_loop = 0

	#Check if maximum number of loops has been breached
	while(solve_loop <= max_solve_loops):

		solve_loop = solve_loop + 1

		print("Solve : " + str(solve_loop))

		#CALCULATE DIRECTION OF PRICE

		#Nload

		#if required load reduction has not yet been reached
		if(Nconstraint == True):
			if(NLoad > NLoad_high):
				
				#Set a new NPrice_min
				if(solve_loop != 1):
					NPrice_min = NPrice
					NPrice_old = NPrice
				
				NPrice = round(NPrice_min + (NPrice_max - NPrice_min)/2,3)
				nsat = False

				if(solve_loop != 1):
					if(NPrice == NPrice_old):
						nsat = True

			#if requered load reduction have been overreached
			if(NLoad < NLoad_low):
				
				#Set a new NPrice_max
				if(solve_loop != 1):
					NPrice_max = NPrice
					NPrice_old = NPrice
				
				NPrice = round(NPrice_min + (NPrice_max - NPrice_min)/2,3)
				nsat = False

				if(solve_loop != 1):
					if(NPrice == NPrice_old):
						nsat = True

			if(NLoad >= NLoad_low and NLoad <= NLoad_high):
				NPrice = NPrice
				nsat = True
		else:
			NPrice = 0.00000
			NPrice_old = 0.00000
			NPrice_min = 0.00000
			NPrice_max = 0.00000
			nsat = True


		#Pload

		#if required load reduction has not yet been reached
		if(Pconstraint == True):
			if(PLoad > PLoad_high):
				
				#Set a new NPrice_min
				if(solve_loop != 1):
					PPrice_min = PPrice
					PPrice_old = PPrice
				
				PPrice = round(PPrice_min + (PPrice_max - PPrice_min)/2,3)
				psat = False

				if(solve_loop != 1):
					if(PPrice == PPrice_old):
						psat = True
			#if requered load reduction have been overreached
			if(PLoad < PLoad_low):
				
				#Set a new NPrice_max
				if(solve_loop != 1):
					PPrice_max = PPrice
					PPrice_old = PPrice					
				
				PPrice = round(PPrice_min + (PPrice_max - PPrice_min)/2,3)
				psat = False

				if(solve_loop != 1):
					if(PPrice == PPrice_old):
						psat = True

			if(PLoad > PLoad_low and PLoad < PLoad_high):
				PPrice = PPrice
				psat = True
		else:
			PPrice = 0.00000
			PPrice_old = 0.0000
			PPrice_min = 0.00000
			PPrice_max = 0.00000
			psat = True

		#Check if model solved
		if(nsat == True and psat == True):
				break
		
		#PREFORM FARM CALCULATIONS
		overview,log = PreformModel(iterations,log,NPrice,PPrice,gams_dir,gams_scenario,gams_frame_path)

		#Sum loads
		z_sum 			= sum([row[1] for row in overview[1:]])
		farmN_sum		= sum([row[2] for row in overview[1:]])
		NLRedFSM_sum	= sum([row[3] for row in overview[1:]])
		NLRedEoFM_sum   = sum([row[4] for row in overview[1:]])
		farmP_sum		= sum([row[5] for row in overview[1:]])
		PLRedFSM_sum	= sum([row[6] for row in overview[1:]])
		PLRedEoFM_sum   = sum([row[7] for row in overview[1:]])

		overview.append(["Total",z_sum,farmN_sum,NLRedFSM_sum,NLRedEoFM_sum,\
										farmP_sum,PLRedFSM_sum,PLRedEoFM_sum,""])

		#Create catchment loads
		kg_to_ton = 1000
		ebit = z_sum / kg_to_ton 
		NLoad = farmN_sum / kg_to_ton
		PLoad = farmP_sum / kg_to_ton

		#ADMINSTRATION

		#Administer time
		time_loop = time.time() - time_previous
		time_passed = time.time() - time_start
		time_previous = time.time()

		#Administer log
		log.write(";".join([str(solve_loop),str(ebit),str(NLoad),str(PLoad),\
		str(NPrice),str(NPrice_min),str(NPrice_max),str(PPrice),str(PPrice_min),\
		str(PPrice_max),str(time_loop),str(time_passed)]) +"\n")



	#ADMINISTRATION

	#Administer log
	log.close()

	#Create overview file
	overview_file = open(gams_dir + gams_scenario + "overview.csv",'w')
	overview_str = [[str(elem) for elem in line] for line in overview]
	overview_lines = [";".join(line) + "\n" for line in overview_str]
	overview_file.writelines(overview_lines)
	overview_file.close()

	return(overview,log)



#RUN

#Read arguments
args = sys.argv[1]
#args = "nitrogen_10red_scenario.ini"

#SET UP MODEL

#Get Setup
gams_dict = readsetup(args)

#Configuration:
gams_dir = gams_dict['gams_dir']
gams_scenario = gams_dict['gams_scenario']
gams_frame_path = "python_GAMS_all_farms_frame.gms"
farms_path = "farms.csv"

#Preform Solve
overview, log = PreformSolve(gams_dict,gams_dir,gams_scenario,farms_path,gams_frame_path)

print("Done.")

