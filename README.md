# GAM.PELT
GAM.PELT has been designed to utilise generalised additive models (GAMs) in conjunction with the Pruned Linear Exact Time (PELT) changepoint detection algorithm to detect spatio-temporal changepoints in datasets.

This a spatiotemporal changepoint method that utilises a generalised additive model (GAM) dependent on the 2D spatial location and the observation time to account for the underlying spatio-temporal process. The full likelihood of the GAM is used in conjunction with the Pruned Linear Exact Time (PELT) changepoint search algorithm to detect multiple changepoints across spatial locations in a computationally efficient manner. Although the method was conceived to detect changepoints in spatio temporal datasets it can be run using any GAM model structure as input.

The repository contains 2 folders **R** which contains all the R scripts to run the GAM-PELT method and **data** which contains the random seeds to replicate the simulation studies and the air quality data used in the application section of the GAM-PELT paper (under review). 

# Running GAM-PELT

In order to run GAM-PELT switch to the directory that contains the R scripts from the **R** folder and run the following code.

```
source("GAM.PELT.R")
```

The only required inputs to run GAM-PELT are a data frame containing the input dataset (**df_in**) and the formula of the GAM model to fit (**formula**). Optional inputs include the minimum segment length for changepoint detection (**minseglen**; default=5), the penalty to use with PELT (**penalty**; default=MBIC, options=AIC, BIC, MBIC, Manual or NONE), penalty value (**pen.value**; default=0.0, only used when penalty is set to Manual) and whether to print progress of method to screen (**verbose**; default=TRUE). The method can also take any options available to the **gam** function in the **mgcv** package with all options set as the defaults.

The output is the GAM-PELT class which provides the following: 

**data.set**  : The data frame containing the input data to GAM-PELT
**pen.type**  : The penalty type used in PELT
**pen.value** : The value of the penalty used in PELT
**minseglen** : The minimum segment length when changepoints are detected
**cpts**      : Changepoint locations calculated by GAM-PELT
**GAM.form**  : The GAM formula supplied to GAM-PELT
**param.est** : The fitted GAM coefficients for each segment based on the changepoint locations

The GAM-PELT class is a standard S4 class with access to the various slots using the names above. E.g. cpts(GAM-PELT class) would output the changepoint locations.


Example code to run GAM-PELT to detect spatio temporal changepoints is shown below. Here the input dataframe is a quantity (Y) located in 2D space (U,V) and at time (T). Here the spatio-temporal process is modelled by a GAM with a 2D thin plate regression spline in space (U,V), a cubic regression spline in time (T) and a tensor interation over space and time. For this example the code is set to look for a minimum segment length of 10 and use the BIC penalty.

```
#Call GAM-PELT using the input dataframe df_in.
cpts_out <- GAM.PELT(df_in, formula='Y ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) + ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5))', minseglen=10, penalty='BIC')

#To access the changepoints estimated by GAM-PELT.
cpts(cpts_out)

#Or to access the penalty value used.
pen.value(cpts_out)

#Or to print a summary of the output to screen.
print(cpts_out)

```

# Reproducing the simulation studies

The **data** folder provides the random seeds used in the GAM-PELT paper to produce the datasets simulation studies for the respective change (1a,1b,1c,2a,2b,2c,3a,3b,3c,4a and 4b) and no change scenarios (A--F). 100 replicates were run for each scenario with the csv file **Sim_study_rseeds_100reps.csv** containing the random seeds in batches of 10 for each scenario. Sample code is shown below how to reproduce the simulated data for a given **SCENARIO** and run GAM_PELT is shown below. The simulated dataset in this example has 50 locations, 200 timesteps with true changpoints at 50,100 and 150.

```
source("GAM.PELT.R")
source("SimStudies_spt.r")

#Generate the simulated dataset using a seed from the file.
sim_ds < gen_SimData(SCENARIO,seed,n_ts=200,n_sites=50,true_cpts=c(50,100,150),add_lag=FALSE)

#Run GAM-PELT on the dataset.
cpts_out <- GAM.PELT(sim_ds$data, formula='Y ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) + ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5))', penalty='BIC')

#Get the changepoints.
cpts(cpts_out)

```

# Data application example

The **data** folder also contains the daily aggregated air quality data from the Automatic Urban and Rural Network (AURN) used in the data application example of GAM-PELT. The folder contains 3 csv files with one for each of the key pollutants O\_3, NO\_2, and PM\_2.5. These data have be aggregated into daily means for the time period 1st February to 31st August 2020 and are ready for running with GAM-PELT. 

The AURN data are freely available to download from the UK-AIR website and provided by the Department for the Environment and Rural Affairs (Defra) under the following attribution statement:

:copyright: Crown copyright 2021 Defra via uk-air.defra.gov.uk, licensed under the [Open Government Licence][https://www.nationalarchives.gov.uk/doc/open-government-licence/version/2/].