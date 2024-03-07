#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/plos_review_01"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/plos_review_01/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/plos_review_01"
# current path
PATH_RETURN=$(pwd)


# ===
# Create directories
# ===
# create output directory:
if [ ! -d ${PATH_OUT} ]; then
	mkdir -p ${PATH_OUT}
fi
# create directory for log files:
if [ ! -d ${PATH_LOG} ]; then
	mkdir -p ${PATH_LOG}
fi


# ===
# Define job parameters for cluster
# ===
# maximum number of cpus per process:
N_CPUS=1
# maximum number of threads per process:
N_THREADS=1
# memory demand in *MB*
MEM_MB=200

# ===
# Set list of conditions to simulate (specific to model)
# ===
#CONDITION_LIST='rwLow rwHigh uncLowPi uncHighPi valLowPos valLowNeg surpriseInc surpriseDec'
CONDITION_LIST='surpriseInc surpriseDec'

# ===
# Set recov parameters
# ===
RANDOM_INPUT_PARAMS="TRUE"
PRINCIPLE_MODE="TRUE"
ITERATIONS=1
TAU=0.2
# No specific betas needed because of principle_mode (sets betas to
# [0,-1,1,NA,NA] if no uncertainty module or [0,-1,1,-1,1] if uncertainty
# module)
BETAS_LB='NA,NA,NA,NA,NA'
BETAS_UB='NA,NA,NA,NA,NA'

# ===
# Run model fitting
# ===

# Loop over different conditions for simulation
for CONDITION in ${CONDITION_LIST}; do

	# 	# Set parameters specifically for models
	# RW
	if [[ ${CONDITION} == 'rwLow' ]]; then
		MODEL='rw'
		PARAM_LB='0.1,NA,NA,NA,NA,NA'
		PARAM_UB='0.2,NA,NA,NA,NA,NA'
		IPS='0.2,NA,NA,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'rwHigh' ]]; then
		MODEL='rw'
		PARAM_LB='0.6,NA,NA,NA,NA,NA'
		PARAM_UB='0.7,NA,NA,NA,NA,NA'
		IPS='0.7,NA,NA,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'uncLowPi' ]]; then
		MODEL='uncertainty'
		PARAM_LB='0.1,0.1,NA,NA,NA,NA'
		PARAM_UB='0.2,0.2,NA,NA,NA,NA'
		IPS='0.2,0.2,NA,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'uncHighPi' ]]; then
		MODEL='uncertainty'
		PARAM_LB='0.1,0.7,NA,NA,NA,NA'
		PARAM_UB='0.2,0.8,NA,NA,NA,NA'
		IPS='0.2,0.8,NA,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'valLowNeg' ]]; then
		MODEL='seplr'
		PARAM_LB='0.5,0.1,NA,NA,NA,NA'
		PARAM_UB='0.6,0.2,NA,NA,NA,NA'
		IPS='0.6,0.2,NA,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'valLowPos' ]]; then
		MODEL='seplr'
		PARAM_LB='0.1,0.5,NA,NA,NA,NA'
		PARAM_UB='0.2,0.6,NA,NA,NA,NA'
		IPS='0.2,0.6,NA,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'surpriseInc' ]]; then
		MODEL='surprise'
		PARAM_LB='0.1,0.7,2,NA,NA,NA'
		PARAM_UB='0.2,0.8,3,NA,NA,NA'
		IPS='0.2,0.8,3,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	elif [[ ${CONDITION} == 'surpriseDec' ]]; then
		MODEL='surprise'
		PARAM_LB='0.7,0.1,2,NA,NA,NA'
		PARAM_UB='0.8,0.2,3,NA,NA,NA'
		IPS='0.8,0.2,3,NA,NA,NA'
		BETA_WEIGHTS='NA,NA,NA,NA,NA'
	fi

	# Get job name
	JOB_NAME="r2c02_sim_model-${MODEL}_principle-${PRINCIPLE_MODE}_cond-${CONDITION}"

	# Create job file
	echo "#!/bin/bash" > job.slurm
	# name of the job
	echo "#SBATCH --job-name ${JOB_NAME}" >> job.slurm
	# set the expected maximum running time for the job:
	echo "#SBATCH --time 23:59:00" >> job.slurm
	# determine how much RAM your operation needs:
	echo "#SBATCH --mem ${MEM_MB}MB" >> job.slurm
	# determine number of CPUs
	echo "#SBATCH --cpus-per-task ${N_CPUS}" >> job.slurm
	# write to log folder
	echo "#SBATCH --output ${PATH_LOG}/slurm-${JOB_NAME}.%j.out" >> job.slurm

	# Load R module
	echo "module unload R" >> job.slurm
	echo "module load R/4.0" >> job.slurm

	# Send command
	echo "Rscript ${PATH_CODE}/r2c02_sim_wrapper.R" \
	--model=${MODEL} \
	--random_input_params=${RANDOM_INPUT_PARAMS} \
	--param_lb=${PARAM_LB} \
	--param_ub=${PARAM_UB} \
	--betas_lb=${BETAS_LB} \
	--betas_ub=${BETAS_UB} \
	--iterations=${ITERATIONS} \
	--tau=${TAU} \
	--ips=${IPS} \
	--beta_weights=${BETA_WEIGHTS} \
	--principle_mode=${PRINCIPLE_MODE} \
	--condition=${CONDITION} >> job.slurm

	# submit job to cluster queue and remove it to avoid confusion:
	sbatch job.slurm
	rm -f job.slurm

done
