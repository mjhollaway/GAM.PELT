# GAM.PELT
GAM.PELT has been designed to utilise generalised additive models (GAMs) in conjunction with the Pruned Linear Exact Time (PELT) changepoint detection algorithm to detect spatio-temporal changepoints in datasets.

This a spatiotemporal changepoint method that utilises a generalised additive model (GAM) dependent on the 2D spatial location and the observation time to account for the underlying spatio-temporal process. The full likelihood of the GAM is used in conjunction with the Pruned Linear Exact Time (PELT) changepoint search algorithm to detect multiple changepoints across spatial locations in a computationally efficient manner. Although the method was conceived to detect changepoints in spatio temporal datasets it can be run using any GAM model structure as input.

The repository contains 2 folders, **R** which contains all the R scripts to run the GAM-PELT method and **data** which contains the random seeds to replicate the simulation studies and the air quality data used in the application section of the GAM-PELT paper (under review). 

# Running GAM-PELT

In order to run GAM-PELT switch to the directory that contains the R scripts from the **R** folder and run the following code.

```
source("GAM.PELT.R")
```

The only required inputs to run GAM-PELT are a data frame containing the input dataset (**df_in**) and the formula of the GAM model to fit (**formula**). Optional inputs include the minimum segment length for changepoint detection (**minseglen**; default=5), the penalty to use with PELT (**penalty**; default=BIC, options=AIC, BIC, MBIC, Manual or NONE), penalty value (**pen.value**; default=0.0, only used when penalty is set to Manual) and whether to print progress of method to screen (**verbose**; default=TRUE). The method can also take any options available to the **gam** function in the **mgcv** package with all options set as the defaults.

The output is the GAM-PELT class which provides the following: 

- **data.set**  : The data frame containing the input data to GAM-PELT
- **pen.type**  : The penalty type used in PELT
- **pen.value** : The value of the penalty used in PELT
- **minseglen** : The minimum segment length when changepoints are detected
- **cpts**      : Changepoint locations calculated by GAM-PELT
- **GAM.form**  : The GAM formula supplied to GAM-PELT
- **param.est** : The fitted GAM coefficients for each segment based on the changepoint locations

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

The **data** folder also contains the daily aggregated air quality data from the Automatic Urban and Rural Network (AURN) used in the data application example of GAM-PELT. The folder contains 3 csv files with one for each of the key pollutants ozone (O<sub>3</sub>), nitrogen dioxide (NO<sub>2</sub>), and particulate matter of diameter less than 2.5 micrometres (PM<sub>2.5</sub>). These data have be aggregated into daily means for the time period 1<sup>st</sup> February to 31<sup>st</sup> August 2020 and are ready for running with GAM-PELT. 

Three files are available containing the datasets used in the application in the paper:

- **AURN_O3_01022020_31082020.csv** - Daily O<sub>3</sub> in $\mu$ gm<sup>-3</sup>  for time period 1<sup>st</sup> February to 31<sup>st</sup> August 2020

- **AURN_NO2_01022020_31082020.csv** - Daily NO<sub>2</sub> in $\mu$ gm<sup>-3</sup>  for time period 1<sup>st</sup> February to 31<sup>st</sup> August 2020

- **AURN_PM_01022020_31082020.csv** - Daily PM<sub>2.5</sub> in $\mu$ gm<sup>-3</sup>  for time period 1<sup>st</sup> February to 31<sup>st</sup> August 2020

Each file contains contains the following variables:

- **U**: Longitude of the station location (in -180.0 t0 180.0 format) (*Used in GAM PELT*)
- **V**: Latitude of the station location (in -90 to 90 format) (*Used in GAM PELT*)
- **T**: Time index of measurement (*Used in GAM PELT*)
- **tstamp**: Time stamp of measurement (in YYYY-MM-DD format) (*for info*)
- **SID**: Site ID of measurement location (*for info*)
- **SCODE**: Site code (*for info*)
- **SFNAME**: Site Name (*for info*)
- **STYPE**: Site type - will be one of Urban Background, Rural Background, Urban Traffic, Urban Industrial, Suburban Industrial or Suburban Background (*for info*)
- **Poll_Value**: Daily concentration of pollutant in $\mu$ gm<sup>-3</sup> (*Used in GAM PELT*)

Example code is shown below to replicate the detection of changepoints in the O3 time series as shown in the GAM_PELT paper.

```
source("GAM.PELT.R")

#Read in the O3 data file.
df_O3 <- read.csv("./data/AURN_O3_01022020_31082020.csv")

#Run GAM-PELT
cpts_O3 <- GAM.PELT(sim_ds$data, formula='Poll_Value ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) + ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5))', penalty='BIC', minseglen=15)

#Output the changepoints.
cpts(cpts_O3)

```