#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="${HOME}/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/pedlr-task/client/public/designs"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/model_recovery"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/model_recovery/$(date '+%Y%m%d_%H%M')"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/model_recovery"
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
# Create zero padded number to indicate design
DATA_LIST=$(seq -f "%02g" 1 1 20)

# ===
# Define job parameters for cluster
# ===
# maximum number of cpus per process:
N_CPUS=1
# maximum number of threads per process:
N_THREADS=1
# memory demand in *GB*
MEM_MB=180
# memory demand in *MB*
#MEM_MB="$((${MEM_GB} * 1000))"

# ===
# Set number of iterative jobs per particpant
# ===
# For more parallelization fitting can be split into multiple jobs per participant
N_PARALLEL=5

# ===
# Run model fitting
# ===
# loop over all design pairs:
for DESIGN_COUNT in ${DATA_LIST}; do

	# create label of participant (full ID without "sub-")
	DESIGN_LABEL="design-${DESIGN_COUNT}"

	# Randomize runs of design
	DESIGN_RUNS=($(seq 1 2 | shuf))
	# Insert random run into designs to shuffle if run1 or run2 is first
  DESIGN_PATH_RUN1="${PATH_DATA}/${DESIGN_LABEL}_run-${DESIGN_RUNS[0]}.tsv"
	DESIGN_PATH_RUN2="${PATH_DATA}/${DESIGN_LABEL}_run-${DESIGN_RUNS[1]}.tsv"

  # Function inputs
	RANDOM_GEN_PARAMETERS="TRUE"
	RANDOM_DESIGN_RUN_ORDER="TRUE"
  N_ITER=2


  # ----------------------------------------------------------------------------
	# ===
	# Rescorla Wagner
	# ===
	# ----------------------------------------------------------------------------
  GEN_MODEL="Rw"
	GEN_PARAMETERS="0.2,7"
  GEN_LB="0,1"
  GEN_UB="1,20"

	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
		 --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

	# ----------------------------------------------------------------------------
	# ===
	# Pedlr_step
	# ===
	# ----------------------------------------------------------------------------
	GEN_MODEL="Pedlr_step"
	GEN_PARAMETERS="0.4,0.6,7"
	GEN_LB="0,0,1"
	GEN_UB="1,1,20"

	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
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
	GEN_MODEL="Pedlr_simple"
	GEN_PARAMETERS="0.4,7"
  GEN_LB="0,1"
  GEN_UB="1,20"

	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
		 --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

	# ----------------------------------------------------------------------------
	# ===
	# Pedlr_simple_const
	# ===
	# ----------------------------------------------------------------------------
	GEN_MODEL="Pedlr_simple_const"
	GEN_PARAMETERS="0.4,7"
  GEN_LB="0,1"
  GEN_UB="1,20"

	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
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
	GEN_MODEL="Pedlr"
	GEN_PARAMETERS="0.2,0.7,7"
	GEN_LB="0,0,1"
	GEN_UB="1,1,20"



	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
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
	GEN_MODEL="Pedlr_fixdep"
	GEN_PARAMETERS="0.2,0.7,7"
	GEN_LB="0,0,1"
	GEN_UB="1,1,20"

	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
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
	GEN_MODEL="Pedlr_interdep"
	GEN_PARAMETERS="0.2,0.7,0.3,7"
	GEN_LB="0,0,0,1"
	GEN_UB="1,1,1,20"

	# Give message to user
	echo "${DESIGN_LABEL}: ${GEN_MODEL}"

  # Loop over job parallelization (N_PARRALEL is zero padded, e.g. 005)
  for PARALLEL in $(seq -f "%03g" ${N_PARALLEL}); do

    # Define output path (depends on parallelization)
    OUTPUT_PATH="${PATH_OUT}/${DESIGN_LABEL}-modelrecov-${GEN_MODEL}-${PARALLEL}.tsv"

		# Get job name
		JOB_NAME="modelrecov_${PARALLEL}_${GEN_MODEL}_${DESIGN_LABEL}"

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

		# Run
		echo "Rscript ${PATH_CODE}/Model_recovery_wrapper.R \
		 --output_path ${OUTPUT_PATH} \
		 --gen_model ${GEN_MODEL} \
		 --design_path_run1 ${DESIGN_PATH_RUN1} \
		 --design_path_run2 ${DESIGN_PATH_RUN2} \
		 --gen_parameters ${GEN_PARAMETERS} \
		 --gen_lb ${GEN_LB} \
		 --gen_ub ${GEN_UB} \
		 --random_gen_parameters ${RANDOM_GEN_PARAMETERS} \
		 --random_design_run_order ${RANDOM_DESIGN_RUN_ORDER} \
		 --n_iter ${N_ITER}" >> job.slurm

  	# submit job to cluster queue and remove it to avoid confusion:
  	sbatch job.slurm
  	rm -f job.slurm
  done

done
