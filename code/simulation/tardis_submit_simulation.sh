#!/usr/bin/bash

# ===
# Define paths
# ===
# define home directory
PATH_BASE="/home/mpib/${USER}/pedlr"
PATH_FUNCTION="${PATH_BASE}/code/simulation/tardis_Apply_model.R"
PATH_DERIVATIVES="${PATH_BASE}/derivatives/simulation"
PATH_LOG="${PATH_BASE}/logs/simulation/$(date '+%Y%m%d_%H%M')"

# ===
# Create directories
# ===
# create output directory:
if [ ! -d ${PATH_DERIVATIVES} ]; then
	mkdir -p ${PATH_DERIVATIVES}
fi
# create directory for log files:
if [ ! -d ${PATH_LOG} ]; then
	mkdir -p ${PATH_LOG}
fi

# ===
# Define job parameters for cluster
# ===
# maximum number of cpus per process:
N_CPUS=2
# maximum number of threads per process:
N_THREADS=2
# memory demand in *GB*
MEM_GB=4

# ===
# Create lists of parameters
# ===
A0_LIST=$(seq -s ' ' 0 1 1)
A1_LIST=$(seq -s ' ' 0 1 1)
TEMPERATURE_LIST=$(seq -s ' ' 1 10 30)
INTERDEP_LIST=0.5
SHRINKAGE_LIST=$(seq -s ' ' 0 10 16)

# ===
# Initialize constant values for simulation
# ===
POLICY="softmax"
REWARD_SPACE=100
MODEL="Pedlr"
INIT_VALUES="50 50 50 50 50 50"
DATA_FILE="${PATH_DERIVATIVES}/simulated_participants.tsv"
OUT_FILE="${PATH_DERIVATIVES}/simulation_${MODEL}"


# ===
# Run simulation
# ===
# initalize a loop counter:
LOOP_COUNT=1
# Loop over alpha0
for A0 in ${A0_LIST}
do
	# Loop over alpha1
	for A1 in ${A1_LIST}
	do
		# Create job to submit
		# name of the job
		echo "#PBS -N ${MODEL}_sim_${LOOP_COUNT}" > job
		# set the expected maximum running time for the job:
		echo "#PBS -l walltime=4:00:00" >> job
		# determine how much RAM your operation needs:
		echo "#PBS -l mem=${MEM_GB}GB" >> job
		# email notification on abort/end, use 'n' for no notification:
		echo "#PBS -m n" >> job
		# write (output) log to log folder
		echo "#PBS -o ${PATH_LOG}" >> job
		# write (error) log to log folder
		echo "#PBS -e ${PATH_LOG}" >> job
		# request multiple cpus
		echo "#PBS -l nodes=1:ppn=${N_CPUS}" >> job

		# inside same job:
		# Loop over temperature
		for TEMP in ${TEMPERATURE_LIST}
		do
			# Loop over Interdependency
			for INTERDEP in ${INTERDEP_LIST}
			do
				# Loop over distance shrinkage
				for SHRINKAGE in ${SHRINKAGE_LIST}
				do
					echo "Rscript \"${PATH_FUNCTION}\" \
					 --data_file \"${DATA_FILE}\" \
					 --model \"${MODEL}\" \
					 --alpha0 $A0 \
					 --alpha1 $A1 \
					 --temperature $TEMP \
					 --reward_space_ub ${REWARD_SPACE} \
					 --choice_policy \"${POLICY}\" \
					 --interdep $INTERDEP \
					 --init_values \"${INIT_VALUES}\" \
					 --distance_shrinkage $SHRINKAGE \
					 --out_file \"${OUT_FILE}_${LOOP_COUNT}.tsv\"" >> job

					 # Iterate loop counter
					 (( LOOP_COUNT++ ))


				 done
			 done
		 done

		 qsub job
		 rm -f job

	 done
 done
