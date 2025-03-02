# CAM Analysis Repository

This repository contains datasets and scripts for replicating Complementary and Alternative Medicine (CAM) analyses, including an Umbrella Review (UR) analysis and a Systematic Review/Meta-Analysis (SR/MA).

## Repository Structure

```
.
├── UR_CAM_analysis.xlsx         # Dataset for Umbrella Review analysis
├── replicating_ur_script.R      # R script for replicating the UR analysis
└── rct_analysis/                # Folder containing RCT analysis files
    ├── RCT_CAM_analysis.xlsx    # Dataset for Randomized Controlled Trials
    └── replicating_ma_script.R  # R script for replicating SR/MA calculations
```

## Umbrella Review Analysis

The root directory contains files to replicate the Umbrella Review (UR) analysis:

- `UR_CAM_analysis.xlsx`: Dataset containing the Umbrella Review data
- `replicating_ur_script.R`: R script with the analysis code to reproduce results

## Systematic Review/Meta-Analysis

The `rct_analysis` folder contains files to replicate the Systematic Review and Meta-Analysis (SR/MA) calculations:

- `RCT_CAM_analysis.xlsx`: Dataset containing Randomized Controlled Trial data
- `replicating_ma_script.R`: R script with analysis code to reproduce SR/MA results

## Usage

1. Clone this repository
2. For UR analysis:
   - Open `replicating_ur_script.R` in R or RStudio
   - Ensure the working directory is set to the repository root
   - Run the script to analyze the `UR_CAM_analysis.xlsx` dataset

3. For SR/MA analysis:
   - Navigate to the `rct_analysis` folder
   - Open `replicating_ma_script.R` in R or RStudio
   - Run the script to analyze the `RCT_CAM_analysis.xlsx` dataset and reproduce the meta-analysis calculations

## Requirements

- R (recommended version: 4.0.0 or newer)
- Required R packages (dependencies are listed in the respective scripts)

## License

[Include your preferred license information here]

## Contact

[Include your contact information here]
