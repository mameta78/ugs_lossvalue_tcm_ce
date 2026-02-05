# Quantifying Recreational Value Losses of Urban Green Spaces under Extreme Heat and Humidity

## Overview
This repository provides replication code and supporting materials for the paper:

Quantifying the loss of recreational value derived from urban green spaces under extreme heat and humidity

The study integrates a Big Data based zonal travel cost model (TCM) with a discrete choice experiment (DCE) to estimate welfare losses from reduced urban green space use under increasing temperature and humidity in Sapporo, Japan.

## Repository Structure

```.
├── CEE_code.R                # Main R script for integrating TCM and DCE results
├── CEE_appendix.R            # Appendix analyses and robustness checks
├── data_R/                   # Processed data for R analysis
├── data_Nlogit/              # Input files for NLOGIT estimation
├── CE_Command.lim            # NLOGIT command file (main model)
├── CE_Command_WBGT.lim       # NLOGIT command file (WBGT specification)
├── CE_Design.ngs             # Ngene experimental design file
├── Read.lim                  # NLOGIT data reading script
├── Read_WBGT.lim             # NLOGIT data reading script (WBGT)
├── Output_RPL_main.lim       # Random parameters logit results
├── Output_RPL_WBGT.lim       # WBGT based RPL results
├── Output_LCM.lim            # Latent class model results
├── Output_continuous.lim     # Continuous specification results
├── Derived_CE_*.lpj          # Derived choice probabilities
└── README.md
```

### Software Requirements
R (>= 4.2)

Required R packages:
tidyverse
data.table
ggplot2

NLOGIT 6 (Econometric Software Inc.) for DCE estimation

Ngene for experimental design (optional)

### Workflow
1.	Discrete choice models (MXL) are estimated in NLOGIT using:
CE_Command.lim
CE_Command_WBGT.lim

2.	Output files from NLOGIT are stored as:
Output_RPL_main.lim
Output_RPL_WBGT.lim
Output_LCM.lim

3.	R scripts are then used to:
integrate DCE results with TCM estimates
calculate changes in visitation probabilities
estimate recreational value losses
generate figures and appendix results

Main entry point:
CEE_code.R

Appendix and robustness analyses:
CEE_appendix.R

### Data Availability
Due to privacy restrictions on mobile phone location data, raw Big Data are not publicly available.

Processed datasets required to reproduce the main results are provided in:
data_R/
data_Nlogit/
