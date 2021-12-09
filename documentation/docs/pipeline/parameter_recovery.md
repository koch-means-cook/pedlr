# Parameter recovery

In this step the behavioral models are fitted to data which was simulated based on the model at hand.
We know the true parameters behind the simulated data and can check how good our fitting process can recover the true parameters.
Basically, this will answer the question: "When we fit a model to the simulated data, will it give us the parameters we used to simulate the data beforehand?".
This produces model-specific data at `.../derivatives/model_fitting`.
This data contains the true parameter values of each simulation as well as the recovered parameters for each specified model.

## 1. Simulate and fit model to simulated data

- `.../code/parameter_recovery/hpc_parameter_recovery.sh`
- Usage: `. hpc_parameter_recovery.sh`
- Starts the jobs for simulating data based on a model and fit the same model to the simulated data on the high computation cluster
   - Submits multiple jobs for each task design (see `N_PARALLEL`)
   - Inside each job there are multiple iterations of the fitting process (see `N_ITER`)
   - Results for each job are saved into `.../derivatives/parameter_recovery/<DESIGN_AND_RUN>-recov-<MODEL_NAME>-<JOB_NUMBER>.tsv`

### No HPC available

- If no HPC is available for preprocessing the fitting process can be done using `.../code/parameter_recovery/Parameter_recovery_wrapper.R`

```
Usage: Parameter_recovery_wrapper.R [options]


Options:
	-o OUTPUT_PATH, --output_path=OUTPUT_PATH
		Path to data file fitting results should be saved to (includes name of target file)

	-m MODEL, --model=MODEL
		Name of model fitting should be based on

	-d DESIGN_PATH, --design_path=DESIGN_PATH
		Path to design file used to simulate data

	-t TRUE_PARAMETERS, --true_parameters=TRUE_PARAMETERS
		List of true parameter values matching the number of parameters of the model. Used to simulate ground truth data. Example: 0.1,0.4,7

	-s START_VALUES, --fit_start_values=START_VALUES
		List of starting values matching the number of parameters of the model. Example: 0.1,0.4,7

	-l LOWER_BOUND, --fit_lb=LOWER_BOUND
		List of values matching the number of parameters of the model giving the lower bound for the fitting process. Example: 0,0,0

	-u UPPER_BOUND, --fit_ub=UPPER_BOUND
		List of values matching the number of parameters of the model giving the upper bound for the fitting process. Example: 1,1,10

	-r RANDOM_TRUE_PARAMETERS, --random_true_parameters=RANDOM_TRUE_PARAMETERS
		TRUE or FALSE if random true parameters should be used

	-f RANDOM_FIT_START_VALUES, --random_fit_start_values=RANDOM_FIT_START_VALUES
		TRUE or FALSE if random start values should be used

	-n N_ITER, --n_iter=N_ITER
		Int giving number of iterations per fitting process. If random_start_values == TRUE new random starting values will be set for each iteration

	-h, --help
		Show this help message and exit
```

## 2. Fuse outputs of multiple parallel fitting jobs

- `.../code/parameter_recovery/Fuse_recov_outputs.R`
- Usage: `Rscript Fuse_recov_outputs.R`
- Automatically detects model fitting results at `.../derivatives/parameter_recovery` and attaches all files of all designs and models into one single file `param_recov.tsv`.
