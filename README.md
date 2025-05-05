### Code and data for quantifying Omicron hidden burden in NL
This repository contains the code to reproduce the main results of the manuscript [Quantifying the Hidden Burden of Omicron and the Impact of Alert Level System in Newfoundland and Labrador](https://). 

### Study Abstract
The Omicron variant of the SARS-CoV-2 virus was highly transmissible and caused unprecedented infections in Newfoundland and Labrador (NL). It is extremely challenging to know the burden of Omicron due to various factors (eg. limited testing capacity, changing testing eligibility and non-pharmaceutical interventions (Nl Alert level System)). The efficacy of the Alert Level System (ALS) implemented in limiting or controlling the spread of the Omicron variant of the SARS-CoV-2 virus in NL is not known. This study considers seroprevalence data to estimate true cumulative SARS-CoV-2 incidence by identifying antibodies that indicate infection with the SARS-CoV-2 virus.  From these seroprevalence data we infer daily incidence (December 15, 2021 to  May 26, 2022) and calibrate a mechanistic compartmental model to these seroprevalence data to estimate the effective reproduction number for different levels of the ALS. Given the testing eligibility criteria, we calculate the eligible fraction of the NL population. We combine these eligibility fraction estimates with the seroprevalence data to independently predict the number of reported cases. We estimate that the net reproduction number was $R_t = ?$ when the most restrictive combination of NPIs (ALS 4) implemented during the study period, was implemented. We found that all other restrictive commbinations reduced the net reproduction number relative to no NPIs, but only ALS-4 was found to contain the spread of the SARS-CoV-2 and reduce the true incidence for a period of time. During the study period, estimated true incidence (178,448) was over four times the number of reported RT-PCR confirmed cases (41,619). Our findings suggest that stringent public health measures can contain Omicron variant spread in highly a vaccinated populations, an outcome that has been observed in only a few jurisdictions. Our approach illustrates how quantities that inform future pandemic preparedness can be estimated even when testing eligibility is restricted and changing.

### Notes on the code
The .R files contain the code to recreate the figures and main statistical analyses. All the codes were written in the R programming language using the [macpan2](https://github.com/canmod/macpan2) package on 1.6 GHz Dual-Core Intel Core i5, 8 GB memory MacBook Air computer. All other packages used in this work can be installed by running the requirement.txt file.

### Data
Omicron-reported case data for NL were obtained through a data-sharing agreement with the Newfoundland and Labrador Centre for Health Information (NLCHI, now part of NL Health Services – Digital Health) under Health Research Ethics Board approval (2021.013). The infection-induced seroprevalence data were obtained from the COVID-19 Immunity Task Force (CITF) website and these studies [1,2]. Information on the RT-PCR testing eligibility criteria was obtained from the public advisory notifications on the Government of Newfoundland and Labrador website [3-7]. 

[1] Murphy, T. J., Swail, H., Jain, J., Anderson, M., Awadalla, P., Behl, L., ... & Buckeridge, D. L. (2023). The evolution of SARS-CoV-2 seroprevalence in Canada: a time-series study, 2020–2023. Cmaj, 195(31), E1030-E1037.

[2] COVID-19 Immunity Task Force. (2023). Seroprevalence of SARS-CoV-2 in Canada: National and regional trends. Government of Canada. https://www.covid19immunitytaskforce.ca/

[3] [GovNL_2021_1215](https://www.gov.nl.ca/releases/2021/health/1215n04/), [4] [GovNL_2022_0103](https://www.gov.nl.ca/releases/2022/health/0103n02/), [5] [GovNL_2022_0124](https://www.gov.nl.ca/releases/2022/health/0124n05/), [6] [GovNL_2022_0225](https://www.gov.nl.ca/releases/2022/health/0309n02/), [7] [GovNL_2022_0317](https://www.gov.nl.ca/releases/2022/health/0317n11/)

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
make eligfrac3.timevar_spec.Rout
```

- **Run the model calibration**  
   This fits the SEARCH-ID model in the timevar_spec to the data and saves the output

```bash
make eligfrac3.calibrate.Rout   
```


- **Load saved output object from the model calibration step and plot**  
   It restores the fitted model from calibrate.rds, including the estimated parameters, generates a plot and saves in figures folder.

```bash
make plot_true_infections.Rout 
```

- **Loads saved model fit output and summarizes the transmission parameter estimates**  
   Extracts the estimated time-varying transmission rate from the calibrated model saved in *.calibrate.rds and generates a plot which gets saved in the figures folder

```bash
make extract_beta.Rout   
```

- **Counterfactual with RT-PCR estimated reporting probabilities**  
First updates the reporting probabilities with eligfrac2 in the timevar_spec model specification for the counterfactual scenario and generates a plot which is saved in the figures folder.

```bash
make eligfrac2.counterfact_sim.Rout   
```

- **All scenario plot**  
Combines reported cases,  estimated true infections and the counterfactual predicted reported with RT-PCR reporting probabilities to generate figure 5.

```bash
make allscenarios.Rout.pdf.go   
```
