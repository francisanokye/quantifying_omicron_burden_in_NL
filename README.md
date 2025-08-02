### Code and data for quantifying Omicron hidden burden in NL
This repository contains the code to reproduce the main results of the manuscript [Quantifying the Hidden Burden of Omicron and the Impact of Alert Level System in Newfoundland and Labrador](https://). 

### Study Abstract
The highly transmissible Omicron variant of SARS-CoV-2 caused many infections in Newfoundland and Labrador, a Canadian province that had previously recorded few COVID-19 cases largely driven by importation rather than widespread community transmission. As diagnostic polymerase chain reaction (PCR) testing capacity was exceeded and testing eligibility criteria changed several more times, reported case counts became unreliable. To mitigate transmission, NL retained its Alert Level System (ALS) of non-pharmaceutical interventions to control the spread with the aim of keeping K–12 schools open whenever possible. Given the limitations of reported case counts, we used infection-induced seroprevalence to estimate population-level infections. We developed a mechanistic compartmental model stratified by vaccination status and calibrated it to infection-induced seroprevalence estimates collected from January 1, 2022, to May 22, 2022. The time-varying transmission rates were modelled using smooth exponential splines. We inferred the mean reproduction numbers across intervention phases and the periods of coincidental school closures and reopenings to quantify the impact of the overlapping ALS on transmission. Our results suggest variable underreporting rates across the study period, but on average one in every five (5) infections was reported by the official surveillance data. Stricter ALS measures, especially under overlapping K–12 closures, reduced transmission, but cases resurged after schools reopened and restrictions were lifted. The results show the usefulness of seroprevalence data and how stringent public health measures reduced Omicron's transmission, while shedding light on the indirect effects of school closures.

### Notes on the code
The .R files contain the code to recreate the figures and main statistical analyses. All the code was written in the R programming language using `version 2.3.4` of the [macpan2](https://github.com/canmod/macpan2) package on 1.6 GHz Dual-Core Intel Core i5, 8 GB memory MacBook Air computer. All other packages used in this work can be installed by running the requirement.txt file.

### Data
Omicron-reported case data for NL were obtained through a data-sharing agreement with the Newfoundland and Labrador Centre for Health Information (NLCHI, now part of NL Health Services – Digital Health) under Health Research Ethics Board approval (2021.013). The infection-induced seroprevalence data were obtained from the COVID-19 Immunity Task Force (CITF) website and these studies [1,2]. Information on the RT-PCR testing eligibility criteria was obtained from the public advisory notifications on the Government of Newfoundland and Labrador website [3-7]. 

[1] Murphy, T. J., Swail, H., Jain, J., Anderson, M., Awadalla, P., Behl, L., ... & Buckeridge, D. L. (2023). The evolution of SARS-CoV-2 seroprevalence in Canada: a time-series study, 2020–2023. Cmaj, 195(31), E1030-E1037.

[2] COVID-19 Immunity Task Force. (2023). Seroprevalence of SARS-CoV-2 in Canada: National and regional trends. Government of Canada. https://www.covid19immunitytaskforce.ca/

[3] [GovNL_2021_1215](https://www.gov.nl.ca/releases/2021/health/1215n04/), [4] [GovNL_2022_0103](https://www.gov.nl.ca/releases/2022/health/0103n02/), [5] [GovNL_2022_0124](https://www.gov.nl.ca/releases/2022/health/0124n05/), [6] [GovNL_2022_0225](https://www.gov.nl.ca/releases/2022/health/0309n02/), [7] [GovNL_2022_0317](https://www.gov.nl.ca/releases/2022/health/0317n11/)

|Data File                       |                       Description                                             |    
|--------------------------------|------------------------------------------------------------------------------------|
|serop_avgcase_data.csv          | contains reported cases and seroprevalence estimates                               |
|rp_eligfrac2.csv                | contains calculated RT-PCR testing eligibility (reporting) fractions and the dates | 
|rp_eligfrac3.csv                | contains reporting probabilities of 1 used to estimate model parameters            | 
|eligibility_adjusted_cases.csv	| data frame saved after using rp_eligfrac2.csv to estimate cases                    | 					
|true_infections_data.csv	      | data frame saved after using rp_eligfrac3.csv to estimate true infections          |
|vaccination-coverage-map.csv    | used to generate the Omicron dominance plot                                        | 
                              


### Reproducing the Results from the Paper

This project uses a `Makefile`-based pipeline to ensure fully reproducible analysis. To generate all results, including calibration, simulations, and plots, follow the steps below.

---

#### Setup Instructions

Before reproducing the results, follow these steps to set up your environment:

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

| Script File                        | Purpose                                                    | 
| ---------------------------------- | ---------------------------------------------------------- | 
| `params.R`                         | Defines model parameter values                             | 
| `keydates.R`                       | Defines key policy/intervention dates                      |
| `flows.R`                          | Computes compartmental flows                               | 
| `spec.R`                           | Builds model specifications                                | 
| `timevar_spec.R`                   | Defines time-varying parameters (e.g., beta, report\_prob) | 
| `seroprevdata.R`                   | Loads or processes seroprevalence data                     | 
| `calibrate.R`                      | Calibrates the model to data                               | 
| `calibrate_plot.R`                 | Plots model fit to data                                    | 
| `extract_beta.R`                   | Extracts fitted β(t) or other parameters                   | 
| `counterfact_sim.R`                | Updates reporting prob & simulates cases                   |
| `allscenarios.R`                   | Combines all the case scenarios                            |
| `Makefile`                         | Streamlines entire analysis pipeline from data to results  |

#### Step-by-Step Pipeline (Makefile Targets)

- **Load model and parameters and key dates**  
   These load and process the baseline parameters:
   Load baseline model specification and parameters and applies the custom reporting fractions in rp_eligfrac3.csv to generates a time-varying spec.

```bash
make timevar_spec.Rout
```

- **Run the model calibration**  
   This fits the SEAIR model in the timevar_spec to the data and saves the output

```bash
make calibrate.Rout   
```

- **Plot model fit with calibrated parameters**  
   Plots the model to the seroprevalence and saves in the figures folder. 

```bash
make plot_model_fit.Rout 
```

- **Use estimated model parameters to estimate the true infections**  
   It restores the fitted model from calibrate.rds, including the estimated parameters, generates a plot and saves in figures folder.

```bash
make plot_true_infections.Rout 
```

- **Loads saved model fit output and summarizes the transmission parameter estimates**  
   Extracts the estimated time-varying transmission rate from the calibrated model saved in *.calibrate.rds and generates a plot which gets saved in the figures folder

```bash
make beta_plot.Rout   
```

- **True infections vrs reported cases plot**  
Plots estimated true infections and the reported cases on the same figure.

```bash
make true_vs_reported_plot.Rout   
```
