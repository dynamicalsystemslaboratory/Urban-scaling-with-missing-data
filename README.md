# Urban-scaling-with-censored-data


## Repository Organization

The folders and files are organized in the following way:

<br>

### topkOptim
"topkOptim" is an R package containing the optimization functions to find $\beta_{min}$ and $\beta_{max}$. To use this package follow these steps:
  1. Download the package;
  2. Move the downloaded package to your R library folder;
  3. The package can be loaded into the R session using library(topkOptim).
  
  The primary functions are:
  1. `optim()`
  2. `optim_aggregate()`
     
For information on how to use these please consult the help files. Note: This package was compiled on a Windows operating system.

<br>

### Firearm_homicides
This folder contains the script "CDC_MSA_MicroSA.R" used in the firearm homicides case study to obtain $\beta_{min}$ and $\beta_{max}$ of firearm homicides in MSAs/MicroSAs in the U.S. 
The spreadsheet used to convert county codes to MSA/MicroSA resolution and the raw data reported by the CDC can be found in the folder "Data" or in the original source, as reported in the manuscript.

<br>

### Recovered Firearms 
This folder contains the script "ATF_States.R" and "ATF_US.R" used in the recovered firearms case study. "ATF_States.R" is a script that uses the function `optim()` to find $\beta_{min}$ and $\beta_{max}$  for each state of the U.S., and "ATF_US.R" uses the function `optim_aggregate()` to find $\beta_{min}$ and $\beta_{max}$  for the U.S.
The raw data used in this analyses reported by the ATF can be found in the folder "Data" or in the original source, as reported in the manuscript. The folder also contains the R script "relevant_data.R" that was used to process the scraped data from the ATF website ("hand_atf.csv") and the population data ("sub-est2022.csv") retrived from the the Census ``Incorporated Places and Minor Civil Divisions Datasets", as reported in the manuscript. 
Additionaly the script "ATF_CI" computes the confidence intervals of $\beta_{min}$ and $\beta_{max}$ for for this case study.

<br>

### Simulations
This folder has three other folders: "Additional_analysis", "Assessing_bias", and "Consistency". The "Additional_analysis" includes scripts for the analyses in Sections S3.2 and S4 of the Supporting Information. The second and third folders contain the script and a copy of the results used in the simulations to assess the bias and the consistency of the $\hat{\beta}^\mathrm{k}$ estimator in urban scaling due to censored data.


