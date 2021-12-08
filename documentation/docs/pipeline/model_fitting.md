# Model fitting

In this step the behavioral models are fitted to the data.
This produces participant-specific and model-specific data at `.../derivatives/model_fitting`.
This data contains parameter values of each participant for each specified model.

## 1. Fit models to participant data

- `.../code/model_fitting/hpc_model_fitting.sh`
- Usage: `. hpc_model_fitting.sh`
- Starts the jobs for model fitting on the high computation cluster
   - Submits multiple jobs for each participant (see `N_PARALLEL`)
   - Inside each job there are multiple iterations of the fitting process (see `N_ITER`)
   - Results for each job are saved into `.../derivatives/model_fitting/<PARTICIPANT_ID>-fit-<MODEL_NAME>-<JOB_NUMBER>.tsv`

### No HPC available

- If no HPC is available for preprocessing the fitting process can be done using `.../code/model_fitting/Fit_model_wrapper.R`

```
Usage: Fit_model_wrapper.R [options]


Options:
	-i INPUT_PATH, --input_path=INPUT_PATH
		Path to data file model should be fit to

	-o OUTPUT_PATH, --output_path=OUTPUT_PATH
		Path to data file fitting results should be saved to (includes name of target file)

	-m MODEL, --model=MODEL
		Name of model fitting should be based on

	-s START_VALUES, --start_values=START_VALUES
		List of starting values matching the number of parameters of the model. Example: 0.1,0.4,7

	-l LOWER_BOUND, --lb=LOWER_BOUND
		List of values matching the number of parameters of the model giving the lower bound for the fitting process. Example: 0,0,0

	-u UPPER_BOUND, --ub=UPPER_BOUND
		List of values matching the number of parameters of the model giving the upper bound for the fitting process. Example: 1,1,10

	-r RANDOM_START_VALUES, --random_start_values=RANDOM_START_VALUES
		TRUE or FALSE if random start values should be used

	-n N_ITER, --n_iter=N_ITER
		Int giving number of iterations per fitting process. If random_start_values == TRUE new random starting values will be set for each iteration

	-h, --help
		Show this help message and exit
```

## 2. Fuse outputs of multiple parallel fitting jobs

- `.../code/model_fitting/Fuse_fitting_outputs.R`
- Usage: `Rscript Fuse_fitting_outputs.R`
- Automatically detects model fitting results at `.../derivatives/model_fitting` and attaches all files of one participant and model into one file `<PARTICIPANT_ID>-fit-<MODEL_NAME>.tsv`
