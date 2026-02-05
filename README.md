# Quantifying Recreational Value Losses of Urban Green Spaces under Extreme Heat and Humidity

## Abstract

This study quantifies recreational value losses of urban green spaces under extreme heat and humidity by integrating a Big Data based zonal travel cost model with a discrete choice experiment. Using mobile phone GPS data and stated preference surveys in Sapporo, Japan, we estimate current recreational benefits and project welfare losses under future climate scenarios. Results show that extreme summer conditions can reduce annual recreational value by more than USD 20 million, while water play features and indoor cooling facilities substantially mitigate these losses. The findings provide policy relevant insights for climate resilient urban planning and adaptation strategies.

### Overview
This repository provides replication code and supporting materials for the paper:

Quantifying the loss of recreational value derived from urban green spaces under extreme heat and humidity

The study integrates a Big Data based zonal travel cost model (TCM) with a discrete choice experiment (DCE) to estimate welfare losses from reduced urban green space use under increasing temperature and humidity in Sapporo, Japan.

## Repository Structure

```.
├── README.md
├── LICENSE
│
├── data/
│   ├── data_Nlogit/                 # Derived outputs from NLOGIT
│   │   ├── Derived_CE_*.csv
│   │   └── Derived_Sap_*.csv
│   │
│   └── data_R/                      # Input and processed data for R
│       ├── CE_KLA_ZTCM/
│       ├── data_appendix10.xlsx
│       └── UGS_info.xlsx
│
├── R/                               # R scripts for integrating TCM and DCE results
│   ├── CEE_code.R                  # Main script
│   └── CEE_appendix.R              # Appendix and robustness analyses
│
├── Nlogit/                          # NLOGIT command, reading, and output files
│   ├── Read.lim
│   ├── Read_WBGT.lim
│   ├── CE_Command.lim
│   ├── CE_Command_WBGT.lim
│   ├── Derived_CE_*.lpj
│   ├── Output_RPL_main.lim
│   ├── Output_RPL_WBGT.lim
│   ├── Output_LCM.lim
│   └── Output_continuous.lim
│
└── Ngene/                           # Experimental design files (optional)
```

## Workflow
1. Discrete choice models (MXL) are estimated in NLOGIT using:

    CE_Command.lim  
    CE_Command_WBGT.lim  

2. Output files from NLOGIT are stored as:

    Output_RPL_main.lim  
    Output_RPL_WBGT.lim  
    Output_LCM.lim  

3. R scripts are then used to:

    integrate DCE results with TCM estimates  
    calculate changes in visitation probabilities  
    estimate recreational value losses  
    generate figures and appendix results  

Main entry point:

    CEE_code.R  

Appendix and robustness analyses:

    CEE_appendix.R

### Software Requirements
R (>= 4.2)

Required R packages:
tidyverse
data.table
ggplot2

NLOGIT 6 (Econometric Software Inc.) for DCE estimation

Ngene for experimental design (optional)

### Data Availability

Due to privacy restrictions on mobile phone location data, raw Big Data are not publicly available.

Processed datasets required to reproduce the main results are provided in:

    data_R/
    data_Nlogit/


