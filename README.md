# Unraveling the Temporal Dynamics of Suicidality, Depression, Irritability, and Connectedness in Psychiatric Inpatients: A Continuous Time Dynamic Modelling Approach for Smartphone-Based Longitudinal Data

This repository contains the code for the study titled "Unraveling the Temporal Dynamics of Suicidality, Depression, Irritability, and Connectedness in Psychiatric Inpatients: A Continuous Time Dynamic Modelling Approach for Smartphone-Based Longitudinal Data". The study explores the interdependence of suicidality, depression, irritability, and connectedness among psychiatric inpatients over time using Hierarchical Bayesian Continuous Time Dynamic Models or Continuous Time Structural Equation Modeling (CTSEM) with Kalman filtering.

## Overview

This project explores the potential of using CTSEM as a powerful tool to examine complex, time-varying mental health constructs, and their reciprocal relationships. Longitudinal data from 30 psychiatric inpatients collected using a smartphone-based app is analyzed, and the project includes an examination of within-person variability and heterogeneity, as well as the estimation of the auto-effect and cross-lagged effects between the mental health constructs.

## Data

The dataset contains the following variables:

The dataset contains the following variables:

- `id`: Participant ID
- `suicidality`: Suicidality scores collected longitudinally 
- `depression`: Depression scores collected longitudinally
- `irritability`: Irritability scores collected longitudinally
- `connectedness`: Connectedness scores collected longitudinally
- `dayOfStudy`: Number of days since the start of the study 
- `startDate`: Start date of the study for each participant 
- `endDate`: End date of the study for each participant 
- `status`: Status of each participant, i.e., whether they completed the study or dropped out.

**Note:** For privacy reasons, actual patient data is not included in this repository.


## Scripts

- `dataCleaning.R`: This script contains the data cleaning and preprocessing steps. 
- `modelFitting_ctsem.R`: This script includes the main analyses using CTSEM.

## Requirements

The analysis was performed using R. Several packages are required to run the scripts, including `ctsem`, `plyr`, `dplyr`, `tidyverse`, `ggplot2`, and others. Please refer to the individual scripts for the full list of required packages.

## Usage

To replicate the analysis, you should run the scripts in the following order:

1. `dataCleaning.R`
2. `modelFitting_ctsem.R`

**Note:** Make sure to adjust file paths and script paths according to your local environment.

## Contact

Please open an issue for support or to report bugs.

## License

This project is licensed under the terms of the USask license.
