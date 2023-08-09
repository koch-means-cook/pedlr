#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/simulation"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/simulation/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/simulation"
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
# Set fitting parameters
# ===
#MODEL_LIST='rw uncertainty seplr uncertainty_seplr surprise uncertainty_surprise'
MODEL_LIST='surprise'
#MODEL_LIST='rw'
#MODEL_LIST='seplr'
#MODEL_LIST='uncertainty_seplr'
TEMPERATURE=7
TAU=0.2

# ===
# Run model fitting
# ===
# loop over all subjects:
for DATA in ${DATA_LIST}; do

	# create label of participant (full ID)
	SUB_LABEL=${DATA:0:7}

  # Function input
  PARTICIPANT_ID="${SUB_LABEL}"

	for MODEL in ${MODEL_LIST}; do
		echo ${MODEL}

		# Set grid parameters for simulation
		if [[ ${MODEL} == 'rw' ]]
		then
			X1_LOW=0.1
			X1_HIGH=0.7
			X1_N=5
			X2_LOW=0.1
			X2_HIGH=0.7
			X2_N=5
			X3_LOW=0
			X3_HIGH=0
			X3_N=0
			X4_LOW=0
			X4_HIGH=0
			X4_N=0
		elif [[ ${MODEL} == 'uncertainty' ]]
		then
			X1_LOW=0.1
			X1_HIGH=0.7
			X1_N=5
			X2_LOW=0.1
			X2_HIGH=0.7
			X2_N=5
			X3_LOW=0
			X3_HIGH=0
			X3_N=0
			X4_LOW=0
			X4_HIGH=0
			X4_N=0
		elif [[ ${MODEL} == 'surprise' ]]
		then
			X1_LOW=0.1
			X1_HIGH=0.7
			X1_N=5
			X2_LOW=0.1
			X2_HIGH=0.7
			X2_N=5
			X3_LOW=0
			X3_HIGH=10
			X3_N=5
			X4_LOW=0
			X4_HIGH=0
			X4_N=0
		elif [[ ${MODEL} == 'uncertainty_surprise' ]]
		then
			X1_LOW=0.1
			X1_HIGH=0.7
			X1_N=5
			X2_LOW=0.1
			X2_HIGH=0.7
			X2_N=5
			X3_LOW=0
			X3_HIGH=10
			X3_N=5
			X4_LOW=0.1
			X4_HIGH=0.7
			X4_N=5
		else
			echo 'No condition applied'
		fi

		# echo ${X1_LOW}
		# echo ${X1_HIGH}
		# echo ${X1_N}
		# echo ${X2_LOW}
		# echo ${X2_HIGH}
		# echo ${X2_N}
		# echo ${X3_LOW}
		# echo ${X3_HIGH}
		# echo ${X3_N}
		# echo ${X4_LOW}
		# echo ${X4_HIGH}
		# echo ${X4_N}

		# Get job name
		JOB_NAME="modelsim_base-${PARTICIPANT_ID}_model-${MODEL}"

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

		echo "Rscript Simulation_wrapper.R \
		--participant_id ${PARTICIPANT_ID} \
		--model ${MODEL} \
		--x1_low ${X1_LOW} --x1_high ${X1_HIGH} --x1_n ${X1_N} \
		--x2_low ${X2_LOW} --x2_high ${X2_HIGH} --x2_n ${X2_N} \
		--x3_low ${X3_LOW} --x3_high ${X3_HIGH} --x3_n ${X3_N} \
		--x4_low ${X4_LOW} --x4_high ${X4_HIGH} --x4_n ${X4_N} \
		--temperature ${TEMPERATURE} \
		--tau ${TAU}" >> job.slurm

		# submit job to cluster queue and remove it to avoid confusion:
		sbatch job.slurm
		rm -f job.slurm

	done
done
