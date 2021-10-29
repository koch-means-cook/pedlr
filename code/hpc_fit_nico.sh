#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/model_fitting/nico"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/model_fitting/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code"
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
# memory demand in *GB*
MEM_MB=128
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"

# ===
# Set number of iterative jobs per particpant
# ===
# For more parallelization fitting can be split into multiple jobs per participant
N_PARALLEL=100

# ===
# Run model fitting
# ===
for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

	# Define output path (depends on parallelization)
	OUTPUT_PATH="${PATH_OUT}/fit-${PARALLEL}.tsv"

	# Get job name
	JOB_NAME="fit_${PARALLEL}"

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

	# Run fitting script
	echo "Rscript ${PATH_CODE}/fit_nico.R \
	--out_file ${OUTPUT_PATH} \
	--random_x0 TRUE" >> job.slurm

	# submit job to cluster queue and remove it to avoid confusion:
	sbatch job.slurm
	rm -f job.slurm

done
