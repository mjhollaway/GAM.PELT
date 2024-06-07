library(mgcv)
library(gstat)
library(msm)

#-----Temporal structure functions-----

#Function to generate the random draw changes in temporal structure of simulation data.
gen_t_all <- function(n_sites,nseg,SIM_SCEN){
   
   #Create a blank matrix to hold the segment parameters - these will be AR, mean and variance.
   t_AR_seg_all   <- matrix(nrow=n_sites,ncol=nseg)
   t_var_seg_all  <- matrix(nrow=n_sites,ncol=nseg)
   t_mean_seg_all <- matrix(nrow=n_sites,ncol=nseg)
   
   #Generate at random what the temporal structure will do between segments.
   #As first segment is fixed need to only generate remaining segments.
   #The available random options will differ depending on whether scenario 4a or 4b.
   #If 4a no change is an option, if 4b no change is not an option.
   if (SIM_SCEN == '4a'){
      t_cng_opts <- c('AR1_all','AR1_one','no_cng')
   } else if (SIM_SCEN == '4b'){
      t_cng_opts <- c('AR1_all','AR1_one')
   }
   t_cng_samp <- c('AR1_all',t_cng_opts[sample.int(length(t_cng_opts), size=nseg-1, replace=TRUE)])
   
   #Now loop over the time segments and generate the AR coefficients for each segment based on the random draw.
   for (curr_t_seg in 1:nseg){

      #If no change just replicate the previous segment.
      if (t_cng_samp[curr_t_seg] == 'no_cng'){
         t_AR_seg_all[,curr_t_seg]   <- t_AR_seg_all[,curr_t_seg-1]
         t_var_seg_all[,curr_t_seg]  <- t_var_seg_all[,curr_t_seg-1]
         t_mean_seg_all[,curr_t_seg] <- t_mean_seg_all[,curr_t_seg-1]
         #All sites change to random AR1.
      } else if (t_cng_samp[curr_t_seg] == 'AR1_all') {
         t_AR_seg_all[,curr_t_seg] <- rtnorm(n_sites, lower=0.1, upper=0.9)
         if (curr_t_seg == 1){
            t_var_seg_all[,curr_t_seg]  <- 1.0
            t_mean_seg_all[,curr_t_seg] <- 0.0           
         } else {
            t_var_seg_all[,curr_t_seg]  <- rlnorm(n_sites, meanlog=0.0, sdlog=log(10)/2)
            t_mean_seg_all[,curr_t_seg] <- rnorm(n_sites, mean=0.0, sd=5.0)
         }
         #One site changes to new random AR1.
      } else if (t_cng_samp[curr_t_seg] == 'AR1_one') {
         #First take a copy of the previous segment.
         t_AR_seg_all[,curr_t_seg]   <- t_AR_seg_all[,curr_t_seg-1]
         t_var_seg_all[,curr_t_seg]  <- t_var_seg_all[,curr_t_seg-1]
         t_mean_seg_all[,curr_t_seg] <- t_mean_seg_all[,curr_t_seg-1]
         #Then change the AR1 coefficient at one site at random.
         site_cng <- sample.int(n_sites, 1)
         t_AR_seg_all[site_cng,curr_t_seg]   <- rtnorm(1, lower=0.1, upper=0.9)
         t_var_seg_all[site_cng,curr_t_seg]  <- rlnorm(1, meanlog=0.0, sdlog=log(10)/2)
         t_mean_seg_all[site_cng,curr_t_seg] <- rnorm(1, mean=0.0, sd=5.0)
      }
   }
   
   return(list(t_AR_seg_all=t_AR_seg_all,t_var_seg_all=t_var_seg_all,t_mean_seg_all=t_mean_seg_all,t_cng_samp=t_cng_samp))
   
}

#Function to generate the temporal structure of the simulated data. 
#This will take in the scenario number and generate the time series part with or without cpts.
gen_t_struct <- function(SIM_SCEN,n_sites,nseg){
   
   #Also create matrix that holds the AR time series parameters for each site.
   #These will vary depending on the scenario run.
   #Also store the type of change between segments (if any) for information.
   if (SIM_SCEN == '1'){
      #Same AR1 parameters at all sites - no change (SCENARIO 1)
      #Mean fixed at 0.0, var fixed at 1.0
      t_AR_seg   <- matrix(rep(rtnorm(1, lower=0.1, upper=0.9),n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_var_seg  <- matrix(rep(1.0,n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_mean_seg <- matrix(rep(0.0,n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_seg_cng  <- 'no_cng_same_AR1_allsites'
   } else if (SIM_SCEN %in% c('2','3a','3b','3c')){
      #Random AR1 parameters at all sites - no change.
      #Mean fixed at 0.0, var fixed at 1.0
      #This structure is used in SCENARIO 2 as well as the spatial change scenarios (3a,3b, and 3c)
      #t_AR_seg  <- matrix(runif(n_sites,0.1,0.9),ncol=1,nrow=n_sites)
      t_AR_seg   <- matrix(rep(rtnorm(n_sites, lower=0.1, upper=0.9),nseg),ncol=nseg,nrow=n_sites)
      t_var_seg  <- matrix(rep(1.0,n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_mean_seg <- matrix(rep(0.0,n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_seg_cng  <- 'no_cng_random_AR1_allsites'
   } else if (SIM_SCEN %in% c('1a','1b','1c')){
      #All sites random AR1 parameters - all sites change (SCENARIO 1a, 1b, 1c)
      #Mean and variance will also change at all sites (initial segment values 0.0 and 1.0 respectively)
      #t_AR_seg  <- matrix(runif(nseg*n_sites,0.1,0.9),ncol=nseg,nrow=n_sites)
      t_AR_seg  <- matrix(rtnorm(nseg*n_sites, lower=0.1, upper=0.9),ncol=nseg,nrow=n_sites)
      #Initialise all sites with a variance of 1.0 for first segment.
      t_var_seg <- matrix(rep(1.0,n_sites),ncol=1,nrow=n_sites)
      t_var_seg <- cbind(t_var_seg,matrix(rlnorm((nseg-1)*n_sites, meanlog=0.0, sdlog=log(10)/2),ncol=(nseg-1),nrow=n_sites))
      #KEEP THE BELOW LINE AT PRESENT IN CASE NEED RANDOM VARIANCES IN ALL SEGMENTS.
      #t_var_seg <- matrix(rlnorm(nseg*n_sites, meanlog=0.0, sdlog=log(10)/2),ncol=nseg,nrow=n_sites)
      #Repeat for the mean intialise at 0.0
      t_mean_seg <- matrix(rep(0.0,n_sites),ncol=1,nrow=n_sites)
      t_mean_seg <- cbind(t_mean_seg,matrix(rnorm((nseg-1)*n_sites, mean=0.0, sd=5.0),ncol=(nseg-1),nrow=n_sites))
      #KEEP THE BELOW LINE AT PRESENT IN CASE NEED RANDOM MEAN IN ALL SEGMENTS.
      #t_mean_seg <- cbind(t_mean_seg,matrix(rnorm((nseg)*n_sites, mean=0.0, sd=5),ncol=(nseg),nrow=n_sites))
      t_seg_cng <- rep('AR1_all',nseg)
   } else if (SIM_SCEN %in% c('2a','2b','2c')){
      #Only one site changes AR parameters at cpt - (SCENARIO 2a, 2b, 2c)
      #First generate a blank matrix to take the segment AR coefficients.
      t_AR_seg   <- matrix(nrow=n_sites,ncol=nseg)
      t_var_seg  <- matrix(nrow=n_sites,ncol=nseg)
      t_mean_seg <- matrix(nrow=n_sites,ncol=nseg)
      #Populate the first segment with same AR coefficient everywhere.
      #Mean and variance will be intialised to 0.0/1.0 respectively.
      #t_AR_seg[,1] <- rep(runif(1,0.1,0.9),n_sites)
      t_AR_seg[,1]   <- rep(rtnorm(1, lower=0.1, upper=0.9),n_sites)
      t_var_seg[,1]  <- 1.0
      t_mean_seg[,1] <- 0.0
      #Now loop over the remaining sites and change 1 site at random.
      #This will be a difference site each time.
      for (ii in 2:nseg){
         #Map the previous segments values and then change 1 at random.
         t_AR_seg[,ii]   <- t_AR_seg[,ii-1]
         t_var_seg[,ii]  <- t_var_seg[,ii-1]
         t_mean_seg[,ii] <- t_mean_seg[,ii-1]
         site_cng <- sample.int(n_sites, 1)
         #t_AR_seg[site_cng,ii] <- runif(1,0.1,0.9)
         t_AR_seg[site_cng,ii]   <- rtnorm(1, lower=0.1, upper=0.9)
         t_var_seg[site_cng,ii]  <- rlnorm(1, meanlog=0.0, sdlog=log(10)/2)
         t_mean_seg[site_cng,ii] <- rnorm(1, mean=0.0, sd=5.0)
      }
      t_seg_cng <- rep('AR1_one',nseg)
   } else if (SIM_SCEN %in% c('4a','4b')){
      #Random draw structure change between segments.
      #Will be one of 3 options (random AR1 all sites, AR1 cng 1 site or no change)
      #If scenario 4b option of no cng will be removed so one of 2 options (random AR1 all sites or AR1 cng 1 site)
      t_AR_param_rand <- gen_t_all(n_sites,nseg,SIM_SCEN)
      t_AR_seg        <- t_AR_param_rand$t_AR_seg_all
      t_var_seg       <- t_AR_param_rand$t_var_seg_all
      t_mean_seg      <- t_AR_param_rand$t_mean_seg_all
      t_seg_cng       <- t_AR_param_rand$t_cng_samp
   } else {
      #For the fixed spatial scenario (3,4,5,6) time structure is fixed as indpendant so just use rnorm here.
      #Therefore the t_AR_seg value will be NULL
      t_AR_seg   <- NULL
      t_var_seg  <- matrix(rep(1.0,n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_mean_seg <- matrix(rep(0.0,n_sites*nseg),ncol=nseg,nrow=n_sites)
      t_seg_cng <- 'no_cng_iid'
   }
      
   return(list(t_AR_seg=t_AR_seg,t_var_seg=t_var_seg,t_mean_seg=t_mean_seg,t_seg_cng=t_seg_cng))
   
}

#-----Spatial structure functions-----

#Function to generate structured correlation for each segment using a spatial model.
gen_SP_corr <- function(vgm_sill, vgm_rng, vgm_nmax, loc_pts, nseg){
   
   vgm_nmax <- min(vgm_nmax,nrow(loc_pts$samp_grd))
   
   #Set up the spatial model based on the input parameters.
   spat_model <- gstat(formula=z~1, ## We assume that there is a constant trend in the data
                       locations=~x+y,
                       dummy=T,    ## Logical value to set to True for unconditional simulation
                       beta=0.0,
                       model=vgm(psill=vgm_sill,range=vgm_rng,nugget=0.0,model='Sph'),
                       nmax=vgm_nmax)
   
   spat_field  <- predict(spat_model, newdata=loc_pts$samp_grd, nsim=nseg)
   #Format to be a matrix of dimension sites (rows) by segments (cols)
   spat_field  <- as.matrix(spat_field[loc_pts$pts,3:ncol(spat_field)])
   #Remove the column and row names
   row.names(spat_field) <- NULL; colnames(spat_field) <- NULL
   
   
   return(spat_field)
   
}

#Function to generate the random draw changes in temporal structure of simulation data.
gen_sp_all <- function(n_sites,nseg,sp_sill,sp_range,sp_nmax,loc_pts,SIM_SCEN){

   #Generate blank matrix to hold the spatial 
   sp_sim_all <- matrix(ncol=nseg,nrow=n_sites)
   
   #Generate at random what the spatial structure will do between segments.
   #As first segment is fixed need to only generate remaining segments.
   #The available random options will differ depending on whether scenario 4a or 4b.
   #If 4a no change is an option, if 4b no change is not an option.
   if (SIM_SCEN == '4a'){
      sp_cng_opts <- c('all_same','all_random','struct','no_cng')
   } else if (SIM_SCEN == '4b'){
      sp_cng_opts <- c('all_same','all_random','struct')
   }
   sp_cng_samp <- c('all_random',sp_cng_opts[sample.int(length(sp_cng_opts), size=nseg-1, replace=TRUE)])
   
   #Now loop over the spatial segments and generate the spatial part of the data.
   for (curr_sp_seg in 1:nseg){
      
      #If no change just replicate the previous segment.
      if (sp_cng_samp[curr_sp_seg] == 'no_cng'){
         sp_sim_all[,curr_sp_seg] <- sp_sim_all[,curr_sp_seg-1]
      } else if (sp_cng_samp[curr_sp_seg] == 'all_same'){
         sp_sim_all[,curr_sp_seg] <- rep(rnorm(n=1,mean=0.0,sd=5.0),n_sites)
      } else if (sp_cng_samp[curr_sp_seg] == 'all_random'){
         sp_sim_all[,curr_sp_seg] <- rnorm(n=n_sites,mean=0.0,sd=5.0)
      } else if (sp_cng_samp[curr_sp_seg] == 'struct'){
         sp_sim_all[,curr_sp_seg] <- gen_SP_corr(sp_sill,sp_range,sp_nmax,loc_pts,1)
      }
   }   
   
   return(list(sp_sim_all=sp_sim_all,sp_cng_samp=sp_cng_samp))
   
}

#Function to generate the 2D bivariate smooth example from GamSim.
#This is based on the code from gamSim in mgcv except adapted to use the location points generated here.
#I.e. The smoothing function utilised in gamSim is used here.
#Smoothing function used in gamSim.
#In difference to gamSim the spatial error is added later.
gen_sp_SCEN6 <- function(x,z,sx=0.3,sz=0.4){ 
   return((pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2) + 0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2)))
}

#Function to generate the spatial structure of the simulated data.
#This will take in the scenario number and generate the time series part with or without cpts. 
gen_sp_struct <- function(SIM_SCEN,loc_pts,n_sites,nseg){
   
   #Generate the parameters for the structured spatial correlation here.
   sp_range <- runif(1,10,1000) 
   sp_sill  <- runif(1,5.0,5000.0) 
   sp_nmax  <- seq(10,200,1)[sample.int(length(10:200), 1)]
   
   
   #Generate the spatial part of the dataset for each site.
   #This will vary depending on the scenario run.
   #Also store description of changes between segments (if any)
   if (SIM_SCEN %in% c('1','2','3','1a','2a')){
      #Fixed constant values for the simulation (SCENARIO 1, 2 and 3) - no change.
      #Also used in scenarios 1a and 2a - changes in time only with constant spatial structure.
      sp_sim     <- matrix(rep(rnorm(n=1,mean=0.0,sd=5.0),n_sites*nseg),ncol=nseg,nrow=n_sites)
      sp_seg_cng <- 'no_cng_constant_allsites'
   } else if (SIM_SCEN %in% c('4','1b','2b')){
      #Independant random values across all sites (SCENARIO 4) - no change.
      #Also used in scenarios 1b and 2b - changes in time only with random everywhere spatial structure.
      sp_sim     <- matrix(rep(rnorm(n=n_sites,mean=0.0,sd=5.0),nseg),ncol=nseg,nrow=n_sites)
      sp_seg_cng <- 'no_cng_random_allsites'
   } else if (SIM_SCEN %in% c('5','1c','2c')){
      #Structured spatial correlation (SCENARIO 5) - no change.
      #Also used in scenarios 1c and 2c - changes in time only with structured spatial structure.
      sp_sim     <- matrix(rep(gen_SP_corr(sp_sill,sp_range,sp_nmax,loc_pts,1),nseg),ncol=nseg,nrow=n_sites)
      sp_seg_cng <- 'no_cng_corrstruct_allsites'
   } else if (SIM_SCEN == '6'){
      #Simulate spatial data from 2D bivariate smoothing GAM example.
      sp_sim     <- matrix(rep(gen_sp_SCEN6(loc_pts$samp_coords$x,loc_pts$samp_coords$y),nseg),ncol=nseg,nrow=n_sites)
      sp_seg_cng <- 'no_cng_gamsim_2D_smooth'  
   } else if (SIM_SCEN == '3a'){
      #Constant everywhere to constant everywhere change (SCENARIO 3a)
      sp_sim     <- matrix(rep(rnorm(n=nseg,mean=0.0,sd=5.0),each=n_sites),ncol=nseg,nrow=n_sites)
      sp_seg_cng <- rep('all_same',nseg)
   } else if (SIM_SCEN == '3b'){
      #Random everywhere to random everywhere change (SCENARIO 3b)
      sp_sim     <- matrix(rnorm(n=nseg*n_sites,mean=0.0,sd=5.0),ncol=nseg,nrow=n_sites)
      sp_seg_cng <- rep('all_random',nseg)
   } else if (SIM_SCEN == '3c'){
      #Structured correlation to structure correlation change (SCENARIO 3c)
      sp_sim     <- gen_SP_corr(sp_sill,sp_range,sp_nmax,loc_pts,nseg)
      sp_seg_cng <- rep('struct',nseg)
   } else if (SIM_SCEN %in% c('4a','4b')){
      #If scenario 4a option of no cng will be removed so one of 4 options (constant across all sites, random at all sites, structured or no change)
      #If scenario 4b option of no cng will be removed so one of 3 options (constant across all sites, random at all sites or structured)
      sp_sim_param_rand <- gen_sp_all(n_sites,nseg,sp_sill,sp_range,sp_nmax,loc_pts,SIM_SCEN)
      sp_sim            <- sp_sim_param_rand$sp_sim_all
      sp_seg_cng        <- sp_sim_param_rand$sp_cng_samp
   }
   
   return(list(sp_seg_param=sp_sim,sp_seg_cng=sp_seg_cng,sp_range=sp_range,sp_nmax=sp_nmax,sp_sill=sp_sill))
   
}

#Function to create the final spatio temporal dataset.
gen_SPT <- function(SIM_SCEN,n_ts,n_sites,true_cpts,loc_pts){


   #All scenarios will produce the same structure output now - all will have paramters for each segment with no changes where no cpt.
   nseg       <- length(true_cpts)+1     
   seg_start  <- c(1,true_cpts+1)
   seg_end    <- c(true_cpts,n_ts)
   
   #Generate the spatial and temporal components of the simulated dataset.
   #The ts_sim_out contains the segment AR1 parameters as well.
   sp_sim_out <- gen_sp_struct(SIM_SCEN,loc_pts,n_sites,nseg)
   ts_sim_out <- gen_t_struct(SIM_SCEN,n_sites,nseg)
      
   #Create an output matrix for the combined spatial and temporal simulated dataset.
   spt_sim <- matrix(nrow=n_ts,ncol=n_sites)

   #Combine the spatial and temporal components - the spatial component will serve as the mean for the arima sim function.
   #For the fixed spatial scenario (3,4,5,6) time structure is fixed as indpendant so just use rnorm here.
   #If the no change scenarios the parameters will be constant across segments so simulate full ts instead rather than segments.
   if (SIM_SCEN %in% c('1','2')){
      for (kk in 1:n_sites){
         spt_sim[,kk] <- arima.sim(list(order=c(1,0,0), ar=ts_sim_out$t_AR_seg[1,1]), n=n_ts, mean=(sp_sim_out$sp_seg_param[1,1] + ts_sim_out$t_mean_seg[1,1]), sd=sqrt(ts_sim_out$t_var_seg[1,1]))
      }
   } else if (SIM_SCEN %in% c('3','4','5','6')){
      for (kk in 1:n_sites){
         spt_sim[,kk] <- rnorm(n_ts,mean=sp_sim_out$sp_seg_param[1,1],sd=5.0)
      }
   } else {
      for (jj in 1:nseg){
         for (kk in 1:n_sites){
             spt_sim[seg_start[jj]:seg_end[jj],kk] <- arima.sim(list(order=c(1,0,0), ar=ts_sim_out$t_AR_seg[kk,jj]), n=length(seg_start[jj]:seg_end[jj]),
                                                                mean=(sp_sim_out$sp_seg_param[kk,jj] + ts_sim_out$t_mean_seg[kk,jj]), sd=sqrt(ts_sim_out$t_var_seg[kk,jj]))
         }   
      }
   }
   
   #Now format the replicate for output.
   #Add some noise to the dataset.
   spt_sim <- spt_sim + matrix(rnorm(n=(nrow(spt_sim)*ncol(spt_sim)),mean=0,sd=0.1*sd(spt_sim)),nrow=nrow(spt_sim),ncol=ncol(spt_sim))
   df_y    <- cbind(as.data.frame(spt_sim),T=1:n_ts) 
   df_y    <- reshape(df_y, direction = 'long', idvar = 'T', list(1:n_sites))   
      
   #New naming convention, spatial dims are U/V, Y is the independant variable.
   df_out  <- as.data.frame(cbind(U=rep(loc_pts$samp_coord[,1],each=n_ts),V=rep(loc_pts$samp_coord[,2],each=n_ts),T=df_y$T,SID=rep(1:n_sites,each=n_ts),Y=df_y$V1))
   
   #Pass out the output to the main calling function.
   #Also pass out the sp and time parameters for each segment - number of parameters for each segment will differ depending on scenario (sp only, t only, spt or no cng).
   return(list(df_out=df_out,sp_param=sp_sim_out$sp_seg_param,t_AR_param=ts_sim_out$t_AR_seg,t_var_param=ts_sim_out$t_var_seg,t_mean_param=ts_sim_out$t_mean_seg,sp_seg_cng=sp_sim_out$sp_seg_cng,
               sp_vgm_range=sp_sim_out$sp_range,sp_vgm_nmax=sp_sim_out$sp_nmax,sp_vgm_sill=sp_sim_out$sp_sill,t_seg_cng=ts_sim_out$t_seg_cng))

}

#Function to generate the sample sites
samp_loc <- function(n_sites){

   #It will be assumed we are working with a dummy 100 by 100 grid for this sample.
   #First generate all potential sample points on a 100 by 100 grid.
   #Also set some dummy decimal coordinates on the grid.
   #all_grd_points        <-  expand.grid(1:100, 1:100)
   all_grd_points  <-  expand.grid(seq(-3.0,3.0,0.1), seq(40.0,60.0,0.1))  #Sample on UK domain.
   names(all_grd_points) <- c('x','y')
   
   #Generate the chosen sample points.
   samp_points    <- sort(sample.int(nrow(all_grd_points), size=n_sites))
   samp_coords    <- data.frame(x=all_grd_points$x[samp_points],y=all_grd_points$y[samp_points])
   samp_grd       <- all_grd_points

   return(list(pts=samp_points,samp_grd=samp_grd,samp_coords=samp_coords))
}

#Create a function that generates the simulation data with changpoint locations.
#Only input will be scenario and initial random seed and replicates.
#Number of time steps will be fixed at 500 with even spaced changepoints at 100,200,300,400.
#Number of sites will be fixed at 50.
gen_SimData <- function(SCENARIO,seed,n_ts=500,n_sites=50,true_cpts=c(100,200,300,400),add_lag=FALSE){

   set.seed(seed)

   #Now generate some sample locations in the spatial field.
   #These will be fixed for all replicates in a given scenario.
   loc_pts <- samp_loc(n_sites)

   #Generate the dataset.
   #Fit the initial GAM to get the simulated results.
   df_sim       <- gen_SPT(SCENARIO,n_ts,n_sites,true_cpts,loc_pts)

   #If lagged values are required to account for autocorrelation bring in here.
   if (add_lag == TRUE){
      #Generate a lag 1 covariate for fitting the GAM.
      df_sim$df_out$Y_LAG1 <- c(rep(NA, 1), df_sim$df_out$Y)[1:length(df_sim$df_out$Y)]
       
      #Now remove where the time is one and reindex the time.
      df_sim$df_out   <- df_sim$df_out[which(df_sim$df_out$T > 1),]
      df_sim$df_out$T <- rep(1:(n_ts-1),n_sites)
   }
   
   return(list(data=df_sim$df_out,sp_param=df_sim$sp_param,t_AR_param=df_sim$t_AR_param,t_var_param=df_sim$t_var_param,t_mean_param=df_sim$t_mean_param,true_cpts=true_cpts,sp_seg_cng=df_sim$sp_seg_cng,
               sp_vgm_nmax=df_sim$sp_vgm_nmax,sp_vgm_range=df_sim$sp_vgm_range,sp_vgm_sill=df_sim$sp_vgm_sill,t_seg_cng=df_sim$t_seg_cng))

}

