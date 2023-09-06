#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/parameter_recovery"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/parameter_recovery/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/parameter_recovery"
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
# Get data
# ===
# User-defined data list
PARTICIPANTS=$1
# Get data to work on
cd ${PATH_DATA}
DATA_LIST=$(ls *_exp_data.tsv)
cd ${PATH_RETURN}
# Only overwrite data with provided input if not empty
if [ ! -z "${PARTICIPANTS}" ]; then
  echo "Specific participant ID supplied"
  # Overwrite sub_list with supplied participant
  DATA_LIST=${PARTICIPANTS}
fi

# ===
# Define job parameters for cluster
# ===
# maximum number of cpus per process:
N_CPUS=1
# maximum number of threads per process:
N_THREADS=1
# memory demand in *GB*
MEM_MB=1000
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"

# ===
# Set list of models to recover
# ===
#MODEL_LIST='rw uncertainty uncertainty_seplr surprise uncertainty_surprise'
MODEL_LIST='uncertainty'

# ===
# Set recov parameters
# ===
RANDOM_INPUT_PARAMS="TRUE"
#RANDOM_INPUT_PARAMS="FALSE"
#RANDOM_INPUT_BETAS="TRUE"
RANDOM_INPUT_BETAS="FALSE"
RANDOM_STARTING_VALUES="TRUE"
#RANDOM_STARTING_VALUES="FALSE"
ALGORITHM="NLOPT_GN_DIRECT_L"
XTOL_REL=0.00001
MAXEVAL=10000
ITERATIONS=5
TAU=0.2

# ===
# Run model fitting
# ===
# loop over all subjects:
for DATA in ${DATA_LIST}; do

	# create label of participant (full ID without "sub-")
	SUB_LABEL=${DATA:0:7}

  # Function input
  PARTICIPANT_ID="${SUB_LABEL}"

	# Loop over different models
	for MODEL in ${MODEL_LIST}; do

		# 	# Set parameters specifically for models
		# RW
		if [[ ${MODEL} == 'rw' ]]; then
			PARAM_LB='0.01,NA,NA,NA'
			PARAM_UB='1,NA,NA,NA'
			IPS='0.2,NA,NA,NA'
			SVS='0.5,NA,NA,NA'
			BETAS_LB='-0.1,-0.5,0,NA,NA'
			BETAS_UB='0.1,0,0.5,NA,NA'
			BETA_WEIGHTS='0,-0.2,0.2,NA,NA'
			# UNCERTAINTY
		elif [[ ${MODEL} == 'uncertainty' ]]; then
			# Full recovery
			PARAM_LB='0.01,0.01,NA,NA'
			PARAM_UB='1,1,NA,NA'
			IPS='0.2,0.7,NA,NA'
			SVS='0.5,0.5,NA,NA'
			BETAS_LB='-0.1,-0.5,0,-0.5,0'
			BETAS_UB='0.1,0,0.5,0,0.5'
			BETA_WEIGHTS='0,-0.2,0.2,-0.1,0.1'
		elif [[ ${MODEL} == 'seplr' ]]; then
			PARAM_LB='0.01,0.01,NA,NA'
			PARAM_UB='1,1,NA,NA'
			IPS='0.2,0.7,NA,NA'
			SVS='0.5,0.5,NA,NA'
			BETAS_LB='-0.1,-0.5,0,NA,NA'
			BETAS_UB='0.1,0,0.5,NA,NA'
			BETA_WEIGHTS='0,-0.2,0.2,NA,NA'
		elif [[ ${MODEL} == 'uncertainty_seplr' ]]; then
			PARAM_LB='0.01,0.01,0,NA'
			PARAM_UB='1,1,1,NA'
			IPS='0.2,0.7,0.7,NA'
			SVS='0.5,0.5,0.5,NA'
			BETAS_LB='-0.1,-0.5,0,-0.5,0'
			BETAS_UB='0.1,0,0.5,0,0.5'
			BETA_WEIGHTS='0,-0.2,0.2,-0.1,0.1'
		elif [[ ${MODEL} == 'surprise' ]]; then
			PARAM_LB='0.01,0.01,0,NA'
			PARAM_UB='1,1,10,NA'
			IPS='0.2,0.7,5,NA'
			SVS='0.5,0.5,0,NA'
			BETAS_LB='-0.1,-0.5,0,NA,NA'
			BETAS_UB='0.1,0,0.5,NA,NA'
			BETA_WEIGHTS='0,-0.2,0.2,NA,NA'
		elif [[ ${MODEL} == 'uncertainty_surprise' ]]; then
			PARAM_LB='0.01,0.01,0,0.01'
			PARAM_UB='1,1,10,1'
			IPS='0.2,0.7,5,0.7'
			SVS='0.5,0.5,0,0.5'
			BETAS_LB='-0.1,-0.5,0,-0.5,0'
			BETAS_UB='0.1,0,0.5,0,0.5'
			BETA_WEIGHTS='0,-0.2,0.2,-0.1,0.1'
		fi

		# Get job name
		JOB_NAME="recov-${PARTICIPANT_ID}_model-${MODEL}_randips-${RANDOM_INPUT_PARAMS}_randbetas-${RANDOM_INPUT_BETAS}_randsvs-${RANDOM_STARTING_VALUES}"

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
		echo "Rscript ${PATH_CODE}/Param_recov_wrapper.R" \
		--participant_id=${PARTICIPANT_ID} \
		--model=${MODEL} \
		--random_input_params=${RANDOM_INPUT_PARAMS} \
		--random_input_betas=${RANDOM_INPUT_BETAS} \
		--random_starting_values=${RANDOM_STARTING_VALUES} \
		--param_lb=${PARAM_LB} \
		--param_ub=${PARAM_UB} \
		--betas_lb=${BETAS_LB} \
		--betas_ub=${BETAS_UB} \
		--algorithm=${ALGORITHM} \
		--xtol_rel=${XTOL_REL} \
		--maxeval=${MAXEVAL} \
		--iterations=${ITERATIONS} \
		--tau=${TAU} \
		--ips=${IPS} \
		--beta_weights=${BETA_WEIGHTS} \
		--svs=${SVS} >> job.slurm

		# submit job to cluster queue and remove it to avoid confusion:
		sbatch job.slurm
		rm -f job.slurm

	done

done
