# Pediction Error Dependent Learning Rate (PEDLR)

This is the repository and documentation fpr the PEDLR project. Involved
scientists are Christoph Koch, Yuanwei Yao, Ondrej Zika, and Nicolas W. Schuck.

---

## Structure

```
.
├── README.md
├── code                                          # Directory containing all code used
│   ├── analysis                                  # Data analysis scripts
│   │   ├── data_quality.Rmd                      # R notebook informing about data quality and suspicious participants
│   │   ├── model_fitting_summary.Rmd             # R notebook informing about model fitting data
│   │   ├── parameter_recovery.Rmd                # R notebook informing about parameter recovery
│   │   ├── summary_designs.Rmd                   # R notebook giving overview of used experiment designs
│   │   └── summary_stats.Rmd                     # R notebook giving summary statistics of data
│   ├── design                                    # Scripts to create experiment designs for task
│   │   ├── Beta_pseudo_sim.R                     # Function to sample beta distribution
│   │   ├── Bimodal_pseudo_sim.R                  # Function to sample bimodal distribution
│   │   ├── Create_block.R                        # Function to create single experiment block
│   │   ├── Create_design.R                       # Function create complete design
│   │   ├── Create_online_design_batch.R          #  Function to create set of designs for online expeirment
│   │   ├── Create_plan.R                         # Function to create plan required to build design (used distributions/blocks/means/...)
│   │   ├── Gaussian_pseudo_sim.R                 # Function to sample Gaussian distribution
│   │   ├── Transform_design_online.R             # Function to transform deisgns to format readable by online experiment (.json)
│   │   └── Uniform_pseudo_sim.R                  # Function to sample uniform distribution
│   ├── model_fitting                             # Scripts used in fitting of behavioral models to participant data
│   │   ├── (...)
│   │   ├── Fit_model.R                           # Script to fit specified model to data
│   │   ├── Fit_model_wrapper.R                   # Wrapper function to make model fitting command-line callable
│   │   ├── Fuse_fitting_outputs.R                # Function to combine fitting outputs
│   │   ├── hpc_model_fitting.sh                  # Shell script to run model fitting on torque-based HPC
│   ├── models                                    # Collection of behavioral models used
│   │   ├── (...)
│   ├── parameter_recovery                        # Scripts used for parameter recovery
│   │   ├── Fuse_recov_outputs.R                  # Script to combine fitting outputs
│   │   ├── Parameter_recovery.R                  # Script to perform parameter recovery
│   │   ├── Parameter_recovery_wrapper.R          # Wrapper function to make parameter recovery command-line callable
│   │   └── hpc_parameter_recovery.sh             # Shell script to run parameter recovery on torque-based HPC
│   ├── preprocessing                             # Scripts used to preprocess raw data of online experiment
│   │   ├── Batch_report.Rmd                      # R markdown pattern of data report for ecah participant
│   │   ├── Calculate_bonus.R                     # Script to calculate bonus payment based on performance
│   │   ├── Create_reports.R                      # Script creating data reports (see Batch_report.Rmd) for raw data points
│   │   ├── Preprocess_arc_data.R                 # Script calling preprocessing for single file
│   │   ├── Preprocess_arc_data_wrapper.R         # Wrapper function to make `Preprocess_arc_data.R` command-line callable
│   │   ├── Preprocess_full_dir.R                 # Wrapper function to preprocess complete dir filled with raw data
│   │   └── Raw_to_data.R                         # Function to preprocess raw data format into format used for analysis
│   └── utils                                     # Folder containing utility functions used across a number of scripts
│       ├── Add_comp.R                            # Function adding bandit comparison types to experiment data (e.g. 1v2, 1v3, 2v3)
│       ├── GetNloptrStatusCodes.R                # Function to save nloptr error status during minimization
│       ├── Get_best_fit.R                        #
│       ├── Get_participants.R                    # Function to get participant codes
│       ├── Load_data.R                           # Function to load experiment data
│       ├── Load_model_fits.R                     # Function to load model fitting data
│       ├── Log_Likelihood.R                      # Likelihood function, minimized by model fitting
│       └── Prepare_data_for_fit.R                # Function to add variables to data needed for fitting
├── documentation                                 # Directory containing all files rendered on docs website
│   └── (...)
├── mkdocs.yml                                    # File needed to render documentation website
```


---

[Documentation website](https://koch.mpib.berlin/pedlr/)

---

Repository profile image: Flaticon.com
