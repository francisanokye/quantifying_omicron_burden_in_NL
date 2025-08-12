### Code and data for quantifying Omicron hidden burden in NL
This repository contains the code to reproduce the main results of the manuscript [Quantifying Omicron’s spread and the impact of non-pharmaceutical interventions in Newfoundland and Labrador](https://). 

## MLi: Don't need abstract

### Abstract
The highly transmissible Omicron variant of SARS-CoV-2 caused many infections in Newfoundland and Labrador (NL), a Canadian province that had previously recorded few COVID-19 cases most of which were imported infections with only few community outbreaks reported. NL retained its Alert Level System (ALS) of non-pharmaceutical interventions to mitigate transmission, while keeping all levels of schooling from kindergarten through Grade 12 (K–12 schools) open as much as possible. Reported case counts became unreliable as diagnostic polymerase chain reaction (PCR) testing capacity was exceeded and testing eligibility criteria changed several times. Using infection-induced seroprevalence estimates from January 1, 2022, to May 22, 2022 to estimate infection, we developed and fit a mechanistic compartmental model stratified by vaccination status to estimate the true number of Omicron infections. We estimated the time-varying transmission rate and the mean reproduction number (![R0](https://latex.codecogs.com/svg.latex?\mathcal{R}_{0})) across overlapping periods of ALS levels and school closure and opening. We found that underreporting varied over time, with **79.5%** of overall infections not captured by official surveillance case counts. Omicron transmission was lower when schools were closed (mean ![R0](https://latex.codecogs.com/svg.latex?\mathcal{R}_{0}) = 1.36, 95% CI: 1.34–1.39) and higher when open (mean ![R0](https://latex.codecogs.com/svg.latex?\mathcal{R}_{0}) = 1.81, 95% CI: 1.49–2.12), with alert level 4 (ALS-4) having the highest effect during K–12 school open periods. Our analysis shows integrating seroprevalence estimates with mechanistic models can reveal infection burden and intervention effects, offering valuable insights for public health decision-making when routine surveillance underestimates true transmission.


### Notes on the code
The .R files contain the code to recreate the figures and main statistical analyses. All the code was written in the R programming language using `version 2.3.4` of the [macpan2](https://github.com/canmod/macpan2) package on 1.6 GHz Dual-Core Intel Core i5, 8 GB memory MacBook Air computer. All other packages used in this work can be installed by running the requirement.txt file.

### Data
Omicron-reported case data for NL were obtained through a data-sharing agreement with the Newfoundland and Labrador Centre for Health Information (NLCHI, now part of NL Health Services – Digital Health) under Health Research Ethics Board approval (2021.013). The infection-induced seroprevalence data were obtained from the COVID-19 Immunity Task Force (CITF) website and these studies [1,2]. Information on the RT-PCR testing eligibility criteria was obtained from the public advisory notifications on the Government of Newfoundland and Labrador website [3-7]. 

[1] Murphy, T. J., Swail, H., Jain, J., Anderson, M., Awadalla, P., Behl, L., ... & Buckeridge, D. L. (2023). The evolution of SARS-CoV-2 seroprevalence in Canada: a time-series study, 2020–2023. Cmaj, 195(31), E1030-E1037.

[2] COVID-19 Immunity Task Force. (2023). Seroprevalence of SARS-CoV-2 in Canada: National and regional trends. Government of Canada. https://www.covid19immunitytaskforce.ca/

[3] [GovNL_2021_1215](https://www.gov.nl.ca/releases/2021/health/1215n04/), [4] [GovNL_2022_0103](https://www.gov.nl.ca/releases/2022/health/0103n02/), [5] [GovNL_2022_0124](https://www.gov.nl.ca/releases/2022/health/0124n05/), [6] [GovNL_2022_0225](https://www.gov.nl.ca/releases/2022/health/0309n02/), [7] [GovNL_2022_0317](https://www.gov.nl.ca/releases/2022/health/0317n11/)

|Data File                       |                       Description                                                  |    
|--------------------------------|------------------------------------------------------------------------------------|
| data/serop_avgcase_data.csv          | contains reported cases and seroprevalence estimates                               |
| data/vaccination-coverage-map.csv    | used to generate the Omicron dominance plot                                        | 
| data/raw_citf_data.csv               | raw CITF seroprevalence estimates                                                  |
|--------------------------------|------------------------------------------------------------------------------------|
### Reproducing the Results from the Paper

This project uses a `Makefile`-based pipeline to ensure fully reproducible analysis. To generate all plots in the results of the manuscript, follow the steps below.

---

#### Setup Instructions

Follow these steps to set up your environment:

- install [macpan2](https://canmod.github.io/macpan2/index.html)

- install [makestuff](https://github.com/dushoff/makestuff)

Install these required packages
```bash
install.packages(c("tidyverse", "ggthemes", "broom.mixed", "conflicted","dplyr", "gridExtra", "grid", "gtable", "zoo", "ggplot2", "patchwork"))
```

##### 1. Clone the Repository

```bash
git clone https://github.com/francisanokye/quantifying_omicron_burden_in_NL.git
cd quantifying_omicron_burden_in_NL/scripts
```

The script folder should contain the following files in the table

| Script File                        | Purpose                                                                           | 
| ---------------------------------- | ----------------------------------------------------------                        | 
| `scripts/params.R`                         | Defines model parameter values                                                    | 
| `scripts/flows.R`                          | Computes compartmental flows                                                      | 
| `scripts/spec.R`                           | Builds model specifications                                                       | 
| `scripts/timevar_spec.R`                   | Builds model specifications  & defines time-varying parameters (e.g., beta)       | 
| `scripts/seroprevdata.R`                   | Loads or processes seroprevalence data                                            | 
| `scripts/calibrate.R`                      | Calibrates the model to data                                                      | 
| `scripts/plot_model_plot.R`                | Plots model fit                                                                   | 
| `scripts/true_vs_reported_plot.R`          | Plots the cumulative reported cases vs estimated infections and the underreporting|
| `scripts/bettas.R`                         | Plots the underlying time-varying transmission rates                              |
| `scripts/reprod_numb.R`                    | Plots the errorbar comparing the different reproduction numbers for the ALS       | 
| `stack_transmission_R0_plot.Rout.R`| Stacks the transmission plot on top of the errorbar                               | 
| `Makefile`                         | Streamlines entire analysis pipeline from data to results                         |
--------------------------------------------------------------------------------------------------------------------------
#### Step-by-Step Pipeline (Makefile Targets)

This pipeline helps to generate the figures for the results used in the paper. It begins by loading and processing the baseline SEAIR model specification and parameters, then generates a time-varying specification for calibration. The model is then fitted to observed data using this time-varying specification, with the calibrated parameters and outputs saved for further analysis. ALways check the figures folder for the plots after run.

- **Fit model to data and plot (Figure_1)**  
```bash
make scripts\plot_model_fit.Rout 
```

- **True infections vrs reported cases plot (cumulative) (Figure_2)**  
Compare the cumulative estimated true infections and the reported cases.

```bash
make true_vs_reported_plot.Rout   
``

```
- **Time-varying transmission rate**  
Generates the transmission rate plot.

```bash
make bettas.Rout   
``

```
- **Error bar plot comparing ALS reproduction numbers**  
Generates the stack of the above two plots.

```bash
make reprod_numb.Rout   
``

```
- **Stacks transmission rate and error bar to generate (Figure_3)**  

```bash
make stack_transmission_R0_plot.Rout   
``
```
- **Generate ALS plots (Figure_4)**  
```bash
make als_R0.Rout   
``
