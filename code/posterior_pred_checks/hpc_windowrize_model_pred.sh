#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/posterior_pred_checks"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/posterior_pred_checks/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/posterior_pred_checks"
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
# Run windowrization
# ===
# loop over all subjects:
for DATA in ${DATA_LIST}; do

	# create label of participant (full ID without "sub-")
	SUB_LABEL=${DATA:0:7}

  # Function input
  PARTICIPANT_ID="${SUB_LABEL}"

	# Get job name
	JOB_NAME="windowrize_postpred-${PARTICIPANT_ID}"

	# Create job file
	echo "#!/bin/bash" > job.slurm
	# name of the job
	echo "#SBATCH --job-name ${JOB_NAME}" >> job.slurm
	# set the expected maximum running time for the job:
	echo "#SBATCH --time 1:00:00" >> job.slurm
	# determine how much RAM your operation needs:
	echo "#SBATCH --mem ${MEM_MB}MB" >> job.slurm
	# determine number of CPUs
	echo "#SBATCH --cpus-per-task ${N_CPUS}" >> job.slurm
	# write to log folder
	echo "#SBATCH --output ${PATH_LOG}/slurm-${JOB_NAME}.%j.out" >> job.slurm

	# Load R module
	echo "module unload R" >> job.slurm
	echo "module load R/4.0" >> job.slurm
	echo "Rscript ${PATH_CODE}/Windowrize_model_pred.R" \
	--participant_id=${PARTICIPANT_ID} >> job.slurm

	# submit job to cluster queue and remove it to avoid confusion:
	sbatch job.slurm
	rm -f job.slurm

done
