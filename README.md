# Data and scripts from paper "Spatio-temporal analysis of mortality from tracheal, bronchial, and lung cancer among women in Argentina, 2000–2023"

This repository contains the complete analytical pipeline, data, and R scripts required to reproduce the statistical findings, mortality estimates, and visualizations presented in the associated research paper.

## Overview
The project is structured as a fully automated workflow. By executing the master script, users can transition from raw data inputs to the final processed datasets and scientific figures used in the manuscript. All spatial and demographic analyses were implemented using the R Programming Language.

## Repository Structure
The directory is organized to maintain a strict separation between raw inputs, processing logic, and analytical outputs:

- [inputs/](https://github.com/agsantoro/tblMortality/tree/main/inputs): Source data required for the analysis.

- [data/mortality/](https://github.com/agsantoro/tblMortality/tree/main/data/mortality): Raw mortality datasets.

- [data/population/](https://github.com/agsantoro/tblMortality/tree/main/data/population): Population counts and socio-demographic covariates.

- shp/: Cartographic boundaries and geospatial data (Shapefiles).

- scripts/: Modular R scripts performing specific tasks such as data cleaning, statistical modeling, and plot generation.

- outputs/: Final results derived from the execution of the pipeline.

- outputs/data/: Processed data frames containing Standardized Mortality Ratios (SMR) and Smoothed Standardized Mortality Ratios.

- outputs/figures/: High-resolution figures and maps as they appear in the publication.

- RUN_PROCESS.r: The master execution script located in the root directory.

## Instructions for Reproduction

To replicate the study results, follow the steps below:

1. Clone or Download this repository to your local environment.

2. Ensure that R is installed.

3. Install the required library dependencies.

4. Run this command in R console:

`
source("RUN_PROCESS.r")
`

Note: The RUN_PROCESS.r script manages the sequential execution of all modular scripts. It reads files from inputs/ and populates the outputs/ folder. Please be aware that existing files in the outputs/ directory will be overwritten upon execution.

## Statistical Methodology

The analytical pipeline implemented in this repository encompasses:

- Data Integration: Merging mortality records with demographic and spatial datasets.

- Indicator Calculation: Computation of crude and adjusted Standardized Mortality Ratios (SMR).

- Spatial Smoothing: Application of spatial models to account for overdispersion and regional autocorrelation.

- Geospatial Visualization: Production of thematic maps and statistical plots using ggplot2 and sf.

## Citation

If you utilize these materials in your research, please cite the original publication:

