#!/usr/bin/bash

# ===
# Define paths
# ===

# define repo directory
PATH_BASE="~/pedlr"
# data directory
PATH_DATA="${PATH_BASE}/data"
# output directory
PATH_OUT="${PATH_BASE}/derivatives/model_fitting"
# directory to save logs of HPC
PATH_LOG="${PATH_BASE}/logs/model_fitting"
# Path to script to run
PATH_CODE="${PATH_BASE}/code/model_fitting"
# current path
PATH_RETURN=pwd


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
N_CPUS=8
# maximum number of threads per process:
N_THREADS=8
# memory demand in *GB*
MEM_GB=35
# memory demand in *MB*
MEM_MB="$((${MEM_GB} * 1000))"
# user-defined subject list
PARTICIPANTS=$1
# Get participants to work on
cd ${PATH_DATA}
DATA_LIST=*.tsv
cd ${PATH_RETURN}

# Only overwrite sub_list with provided input if not empty
if [ ! -z "${PARTICIPANTS}" ]; then
  echo "Specific participant ID supplied"
  # Overwrite sub_list with supplied participant
  SUB_LIST=${PARTICIPANTS}
fi

echo ${DATA_LIST}
