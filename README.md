# CAM Analysis Repository

This repository contains datasets and scripts for replicating EBIA-CT Complementary and Alternative Medicine (CAM) analyses

## Repository Structure

```
.
├── UR_CAM_analysis.txt            # Dataset with Umbrella Review results, useful to visualize the results
├── index_cam.Rmd                  # R script for replicating the UR analysis
└── full_replication_materials/    # Folder containing RCT analysis files
    ├── sr_ma_analysis.R           # R script for analyzing the RCTs and generating meta-analytic evidence
    ├── cct_level_CAM.txt          # Dataset for Randomized Controlled Trials
    ├── list_studies.txt           # List of included/excluded studies
    ├── prior_checklist.xlsx       # PRIOR checklist
    ├── Search.pdf                 # Search strategies on the different databases
    └── replicating_ma_script.Rmd  # R script for replicating SR/MA calculations
```

## Umbrella Review Analysis

The root directory contains files to visualize the Umbrella Review (UR) analysis:

- `UR_CAM_analysis.txt`: Dataset containing the Umbrella Review data
- `index_cam.R`: R script with the analysis code to reproduce results

## Complete results

The `full_replication_materials` folder contains all files generated during this umbrella review, from the list of included studies to the individual RCTs data allowing to replicate all meta-analytic calculations:

- `replicating_ma_script.R`: This script automatically analyzes RCT data

## Usage

1. Clone this repository
2. For UR analysis:

   - Open `replicating_ur_script.R` in R or RStudio
   - Ensure the working directory is set to the repository root
   - Run the script to analyze the `UR_CAM_analysis.xlsx` dataset

3. For SR/MA analysis:
   - Navigate to the `rct_analysis` folder
   - Open `replicating_ma_script.R` in R or RStudio

## Requirements

- R (recommended version: 4.0.0 or newer)
- Required R packages (dependencies are listed in the respective scripts)
- The most important R package is metaumbrella v1.1.0 to benefit from the new GRADE scoring.

## License

-

## Contact

Dr Corentin J. Gosling [cgosling@parisnanterre.fr]
