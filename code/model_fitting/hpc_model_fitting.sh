#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/model_fitting"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/model_fitting/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/model_fitting"
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
DATA_LIST=$(ls *.tsv)
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
MEM_MB=128
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"

# ===
# Set number of iterative jobs per particpant
# ===
# For more parallelization fitting can be split into multiple jobs per participant
N_PARALLEL=10

# ===
# Run model fitting
# ===
# loop over all subjects:
for DATA in ${DATA_LIST}; do

	# create label of participant (full ID without "sub-")
	SUB_LABEL=${DATA:0:7}

  # Function inputs
  INPUT_PATH="${PATH_DATA}/${DATA}"
	RANDOM_START_VALUES="TRUE"
  N_ITER=10


  # ----------------------------------------------------------------------------
	# ===
	# Rescorla Wagner
	# ===
	# ----------------------------------------------------------------------------
  MODEL="Rw"
  START_VALUES="0.5,5"
  LB="0,1"
  UB="1,10"

	# Give message to user
	echo "${SUB_LABEL}: ${MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${SUB_LABEL}-fit-${MODEL}-${PARALLEL}.tsv"

  	# Get job name
  	JOB_NAME="fit_${PARALLEL}_${MODEL}_${SUB_LABEL}"

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
    echo "Rscript ${PATH_CODE}/Fit_model_wrapper.R \
    --input_path ${INPUT_PATH} \
    --output_path ${OUTPUT_PATH} \
    --model ${MODEL} \
    --start_values ${START_VALUES} \
    --lb ${LB} \
    --ub ${UB} \
    --random_start_values ${RANDOM_START_VALUES} \
    --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

	# ----------------------------------------------------------------------------
	# ===
	# Pedlr_simple
	# ===
	# ----------------------------------------------------------------------------
	MODEL="Pedlr_simple"
	START_VALUES="0.5,5"
	LB="0,1"
	UB="1,10"

	# Give message to user
	echo "${SUB_LABEL}: ${MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${SUB_LABEL}-fit-${MODEL}-${PARALLEL}.tsv"

  	# Get job name
  	JOB_NAME="fit_${PARALLEL}_${MODEL}_${SUB_LABEL}"

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
    echo "Rscript ${PATH_CODE}/Fit_model_wrapper.R \
    --input_path ${INPUT_PATH} \
    --output_path ${OUTPUT_PATH} \
    --model ${MODEL} \
    --start_values ${START_VALUES} \
    --lb ${LB} \
    --ub ${UB} \
    --random_start_values ${RANDOM_START_VALUES} \
    --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

  # ----------------------------------------------------------------------------
	# ===
	# Pedlr
	# ===
	# ----------------------------------------------------------------------------
	MODEL="Pedlr"
	START_VALUES="0.5,0.5,5"
	LB="0,0,1"
	UB="1,1,10"

	# Give message to user
	echo "${SUB_LABEL}: ${MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${SUB_LABEL}-fit-${MODEL}-${PARALLEL}.tsv"

  	# Get job name
  	JOB_NAME="fit_${PARALLEL}_${MODEL}_${SUB_LABEL}"

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
    echo "Rscript ${PATH_CODE}/Fit_model_wrapper.R \
    --input_path ${INPUT_PATH} \
    --output_path ${OUTPUT_PATH} \
    --model ${MODEL} \
    --start_values ${START_VALUES} \
    --lb ${LB} \
    --ub ${UB} \
    --random_start_values ${RANDOM_START_VALUES} \
    --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

	# ----------------------------------------------------------------------------
	# ===
	# Pedlr_fixdep
	# ===
	# ----------------------------------------------------------------------------
	MODEL="Pedlr_fixdep"
	START_VALUES="0.5,0.5,5"
	LB="0,0,1"
	UB="1,1,10"

	# Give message to user
	echo "${SUB_LABEL}: ${MODEL}"

	# Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
	for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

		# Define output path (depends on parallelization)
		OUTPUT_PATH="${PATH_OUT}/${SUB_LABEL}-fit-${MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="fit_${PARALLEL}_${MODEL}_${SUB_LABEL}"

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
		echo "Rscript ${PATH_CODE}/Fit_model_wrapper.R \
		--input_path ${INPUT_PATH} \
		--output_path ${OUTPUT_PATH} \
		--model ${MODEL} \
		--start_values ${START_VALUES} \
		--lb ${LB} \
		--ub ${UB} \
		--random_start_values ${RANDOM_START_VALUES} \
		--n_iter ${N_ITER}" >> job.slurm

		# submit job to cluster queue and remove it to avoid confusion:
		sbatch job.slurm
		rm -f job.slurm
	done

  # ----------------------------------------------------------------------------
	# ===
	# Pedlr_interdep
	# ===
	# ----------------------------------------------------------------------------
	MODEL="Pedlr_interdep"
	START_VALUES="0.5,0.5,0.5,5"
	LB="0,0,0,1"
	UB="1,1,1,10"

	# Give message to user
	echo "${SUB_LABEL}: ${MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${SUB_LABEL}-fit-${MODEL}-${PARALLEL}.tsv"

  	# Get job name
  	JOB_NAME="fit_${PARALLEL}_${MODEL}_${SUB_LABEL}"

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
    echo "Rscript ${PATH_CODE}/Fit_model_wrapper.R \
    --input_path ${INPUT_PATH} \
    --output_path ${OUTPUT_PATH} \
    --model ${MODEL} \
    --start_values ${START_VALUES} \
    --lb ${LB} \
    --ub ${UB} \
    --random_start_values ${RANDOM_START_VALUES} \
    --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

done
