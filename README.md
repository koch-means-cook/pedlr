# Pediction Error Dependent Learning Rate (PEDLR)

This is the repository and documentation for the PEDLR project. Involved
scientists are Christoph Koch, Ondrej Zika, Rasmus Bruckner, and Nicolas W. Schuck.

---

## Structure

```
├── code
│   ├── analysis
│   │   ├── data_quality.Rmd                      # Overview of data quality
│   │   ├── demographic.Rmd                       # Overview of sample demographic
│   │   ├── Render_data_quality.R                 # Function rendering `data quality.Rmd` into derivatives
│   │   ├── Render_demographic.R                  # Function rendering `demographic.Rmd` into derivatives
│   │   ├── Render_results_01.R                   # Function rendering `results_01.Rmd` into derivatives
│   │   ├── Render_results_02.R                   # Function rendering `results_02.Rmd` into derivatives
│   │   ├── results_01.Rmd                        # Main analysis script for all behavioral results
│   │   └── results_02.Rmd                        # Main analysis script for all modelling results
│   ├── design                                    # Scripts to create experiment designs for task
│   │   ├── Beta_pseudo_sim.R                     # Function to sample beta distribution
│   │   ├── Bimodal_pseudo_sim.R                  # Function to sample bimodal distribution
│   │   ├── Create_block.R                        # Function to create single experiment block
│   │   ├── Create_design.R                       # Function create complete design
│   │   ├── Create_online_design_batch.R          # Function to create set of designs for online expeirment
│   │   ├── Create_plan.R                         # Function to create plan required to build design (used distributions/blocks/means/...)
│   │   ├── Gaussian_pseudo_sim.R                 # Function to sample Gaussian distribution
│   │   ├── Transform_design_online.R             # Function to transform deisgns to format readable by online experiment (.json)
│   │   └── Uniform_pseudo_sim.R                  # Function to sample uniform distribution
│   ├── figures
│   │   ├── Create_figures.R                      # Function to create all figures
│   │   └── (...)
│   ├── model_fitting                             # Directory holding model fitting functions
│   │   ├── Compute_value.R                       # Function to compute values of bandits during model fitting
│   │   ├── Fit_models_new.R                      # Main function to fit all models choice data
│   │   ├── Fit_models_new_wrapper.R              # Wrapper function to fit models
│   │   ├── Fuse_fitting_outputs.R                # (deprecated)
│   │   ├── hpc_model_fitting_new.sh              # Submit script to run model fiting on HPC (Slurm)
│   │   ├── LRfunction.R                          # Script to calculate LR based on l,u,s parameters
│   │   └── Regression_model.R                    # Script with regression model used in fitting procedure
│   ├── parameter_recovery                        # Directory holding all functions for parameter and model recovery
│   │   ├── analysis_model_recov.qmd              # Analysis script for model recovery results
│   │   ├── analysis_param_recov.qmd              # Analysis script for parameter recovery results
│   │   ├── hpc_param_recov.sh                    # Parameter recovery submit script to HPC (Slurm)
│   │   ├── Param_recov.R                         # Main function running parameter and model recovery
│   │   ├── Param_recov_wrapper.R                 # Wrapper function to perform parameter and model recovery
│   │   ├── Render_analysis_model_recov.R         # Function rendering `analysis_model_recov.qmd` into derivatives
│   │   └── Render_analysis_param_recov.R         # Function rendering `analysis_parameter_recov.qmd` into derivatives
│   ├── plos_review_01                            # Directory holding all analyses made during plos review round 1
│   │   └── (...)                                 # Scripts are labelled specifically for reviewer comments
│   ├── posterior_pred_checks                     # Directory holding alls functions for post. pred. checks
│   │   ├── analysis_posterior_pred_checks.qmd    # Analysis script for post. pred. checks
│   │   ├── Get_fit.R                             # Function to get fitted parameters for each participant
│   │   ├── hpc_posterior_prediction.sh           # HPC submit script for post pred checks
│   │   ├── hpc_windowrize_model_pred.sh          # HPC submit script to calculate critical behavioral effect
│   │   ├── Posterior_prediction.R                # Main function to run post pred checks
│   │   ├── Render_analysis_posterior_pred_checks.R # Function rendering `analysis_posterior_pred_checks.qmd` into derivatives
│   │   └── Windowrize_model_pred.R               # Function to calculate critical behavioral effect for simulated data
│   ├── preprocessing                             # Scripts used to preprocess raw data of online experiment
│   │   ├── Batch_report.Rmd                      # R markdown pattern of data report for ecah participant
│   │   ├── Calculate_bonus.R                     # Script to calculate bonus payment based on performance
│   │   ├── Create_reports.R                      # Script creating data reports (see Batch_report.Rmd) for raw data points
│   │   ├── Preprocess_arc_data.R                 # Script calling preprocessing for single file
│   │   ├── Preprocess_arc_data_wrapper.R         # Wrapper function to make `Preprocess_arc_data.R` command-line callable
│   │   ├── Preprocess_full_dir.R                 # Wrapper function to preprocess complete dir filled with raw data
│   │   └── Raw_to_data.R                         # Function to preprocess raw data format into format used for analysis
│   ├── simulation                                # Directory holding all functions to simulate data from a model
│   │   ├── Compute_value_per_trial.R             # Function to compute value for each trial to simulate data and choices
│   │   ├── hpc_simulation.sh                     # HPC submit script to run simulation
│   │   ├── hpc_simulation_windowrize.sh          # HPC scubmit script to calculate CBE in simulations
│   │   ├── Make_choice.R                         # Function to simulate choices based on model and bandit values
│   │   ├── Render_simulation_analysis.R          # Function rendering `simulation_analysis.qmd` into derivatives
│   │   ├── Simulate.R                            # Main function to simulate data from a model
│   │   ├── simulation_analysis.qmd               # (deprecated)
│   │   ├── Simulation_windowrize.R               # Function to calculate CBE in simulated data
│   │   └── Simulation_wrapper.R                  # Wrapper function to run simulation for different models and data
│   └── utils                                     # Directory holding utility functions sourced in other scripts
│       ├── Add_comp.R                            # Function to add bandit comparison to data table (e.g. 1v2, for low-mid)
│       ├── Apply_exclusion_criteria.R            # Function to apply exclusion criteria specified in manuscript
│       ├── Collapse_repeated_rows.R              # Function to render tables
│       ├── Get_exclusions.R                      # Function to get list of participants that are excluded
│       ├── GetNloptrStatusCodes.R                # Function to interpret optimization status codes in data tables
│       ├── Get_participants.R                    # Function to load participant names
│       ├── Get_plot_guides.R                     # Function to create own guide table for (gg)-plotting
│       ├── Get_running_avg.R                     # Function to calculate running average of bandit
│       ├── Get_sampling.R                        # Function to get sampling of each design
│       ├── Get_svs.R                             # Function to get starting values during fitting processes
│       ├── LL_reg.R                              # Likelihood function (optimized during model fitting)
│       ├── Load_data.R                           # Function to load choice data of participants from `data`
│       ├── Load_model_fits_new.R                 # Function to load model fits from `derivatives`
│       ├── Neurocodify_plot.R                    # Plot design function (ggplot theme)
│       ├── Prepare_data_for_plot.R               # Function to rename variables for plots (for readability)
│       ├── Render_to_derivatives.R               # General function to render Rmd/qmd files to derivatives
│       └── RFreeze.R                             # (deprecated)
├── Dockerfile
├── mkdocs.yml
├── pedlr.Rproj
├── README.md
├── renv.lock                                     # Lockfile listing all required R packages abd R version
└── requirements.txt
```
