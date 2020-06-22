#!/usr/bin/bash

# ===
# Define paths
# ===
# define home directory
PATH_BASE="/home/mpib/${USER}/pedlr"
PATH_DERIVATIVES="${PATH_BASE}/derivatives/simulation"

# ===
# Create directories
# ===
# create output directory:
if [ ! -d ${PATH_DERIVATIVES} ]; then
	mkdir -p ${PATH_DERIVATIVES}
fi

# Go into derivatives where simulations are stored
cd ${PATH_DERIVATIVES}

if test -f "sim_Pedlr_000001.tsv"; then
	echo "Concatenating sim_Pedlr files..."
	echo sim_Pedlr_*.tsv | xargs cat > sim_Pedlr.tsv
	echo "Deleting incremental files..."
	rm sim_Pedlr_*.tsv
else
	echo "No sim_Pedlr_000001.tsv in directory."
	echo "Abort."
fi
