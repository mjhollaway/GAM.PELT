#Create a list of datasets.
#Initially generate a base dataset to work from - 100 timesteps with on cpt at 50.
base_ds <- gen_SimData('4b',1234,n_ts=100,n_sites=50,true_cpts=c(50))

ds_not_df    <- base_ds$data$U  #Single time series from base_ds
ds_full      <- base_ds$data    #Correct input format.
#Data frame with diffent variables for GAM formula (change X/Y to A/B)
ds_wrong_var             <- ds_full
names(ds_wrong_var)[1:2] <- c('A','B')
#Data frame with too many Nans for a meaningful GAM fit (set entire U variable to NaN).
ds_nan   <- ds_full
ds_nan$U <- NaN
#Data frame with random NaNs in the data set for a couple of variables.
ds_rnd_nan <- ds_full
ds_rnd_nan$U[1200:1300] <- NaN
ds_rnd_nan$V[2500:2700] <- NaN
#Data frame with non-numeric data in a single column.
ds_nonnum   <- ds_full
ds_nonnum$U[1000:2000] <- "Missing"
#Data frame with one covariate set to the same value.
ds_1cov_same   <- ds_full
ds_1cov_same$U <- 10.0
#Data frame with two covariatea set to the same value.
ds_2cov_same   <- ds_full
ds_2cov_same$U <- 10.0
ds_2cov_same$V <- 10.0

#Combine into a named list to loop over.
ds_test_list        <- list(ds_not_df,ds_wrong_var,ds_nan,ds_rnd_nan,ds_nonnum,
                            ds_1cov_same,ds_2cov_same,ds_full)
names(ds_test_list) <- c('Not_dataframe','Wrong_variables','Too_many_NaNs',
                         'Random_NaNs','Non_Numeric','1_Cov_same','2_cov_same',
                         'Correct_format')

#Set the GAM formulae for the tests - C,D rather than X,Y
GAM_formula_wrongvar    <- Y ~ s(C, D, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) +
  ti(C,D, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5))

GAM_formula_correct     <- Y ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) +
  ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5))

GAM_formula_notformula  <- 'Not formula'
GAM_formula_list        <- list(GAM_formula_wrongvar,GAM_formula_notformula,GAM_formula_correct)
names(GAM_formula_list) <- c('Wrong_formula','Not_formula','Correct_formula')

#Set a list of penalties to test.
penalties_tests <- list('None', 'MBIC', 'BIC', 'SIC', 'MBIC', 'AIC', 'Manual', 'bic',
                        'mbic', 'Asymptotic', 'MIBC','Mnual')

#Penalty values.
penalties_values <- list(-1,'-200','VALUE','220.0',220.0)

#Verbose check.
verbose_tests   <- list(TRUE,FALSE,'NOT LOGICAL')

#Fit keyword check.
GAM_fit_tests   <- list(TRUE,FALSE,'NOT LOGICAL')

#Minimum segment length values.
minseglen_tests <- list(60,10,45,'Segment_length',10.5,-1)

#Set a counter to keep track of the test no.
t <- 1

#Function to check the manual penalty value with different values.
checkManualPenalty <- function(){

  #Loop over the various manual penalty values and check they return the expected errors.
  for (mpv in 1:length(penalties_values)){
    #This check tries to see if a number was passed as a string and retrives the penalty value.
    #If passes a numeric penalty value will be returned to check with GAM.PELT.
    if (is.numeric(penalties_values[[mpv]]) == FALSE){
      texttest <- try(eval(parse(text=paste(penalties_values[[mpv]]))),silent=TRUE)
      if (inherits(texttest,'try-error')){
        test_that('Manual penalty cannot be evaluated', {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                                verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]],
                                penalty=penalties_tests[[pp]] , pen.value=penalties_values[[mpv]]),
                       "Manual penalty could not be evaluated.")
          t <- t + 1
        })
      } else if (texttest < 0){
        test_that('Negative penalty', {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                                verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]],
                                penalty=penalties_tests[[pp]] , pen.value=penalties_values[[mpv]]),
                       "pen.value cannot be negative, please change your penalty value.")
          t <- t + 1
        })
      } else if (texttest > 0){
        test_that('Manual penalty correct', {
          X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                        verbose=verbose_tests[[vv]],minseglen=45,
                        penalty=penalties_tests[[pp]],pen.value=penalties_values[[mpv]])
          expect_s4_class(X,'GAM_PELT')
          t <- t + 1
        })
      }
      #Check to see if numeric penalty value is negative or not.
    } else {
      if (penalties_values[[mpv]] < 0){
        test_that('Negative penalty', {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                                verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]],
                                penalty=penalties_tests[[pp]] , pen.value=penalties_values[[mpv]]),
                       "pen.value cannot be negative, please change your penalty value.")
          t <- t + 1
        })
      } else {
        test_that('Manual penalty correct', {
          X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                        verbose=verbose_tests[[vv]],minseglen=45,
                        penalty=penalties_tests[[pp]],pen.value=penalties_values[[mpv]])
          expect_s4_class(X,'GAM_PELT')
          t <- t + 1
        })
      }
    }
  }
}

#Loop over test scenarios and run tests.
for (dd in 1:length(ds_test_list)){
  for (gg in 1:length(GAM_formula_list)){

    #Check the various dataset/GAM formula combinations.
    #If GAM formula is correct and data wrong - run data checks
    if ((names(ds_test_list)[dd] != 'Correct_format') & (names(GAM_formula_list)[gg] == 'Correct_formula')){

      if (names(ds_test_list)[dd] == 'Not_dataframe'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]], formula=GAM_formula_list[[gg]]),"Input data is not a data.frame")
          t <- t + 1
        })
      } else if (names(ds_test_list)[dd] == 'Wrong_variables'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Input data.set missing variables required by formula. Please check both formula and dataset.")
          t <- t + 1
        })
      } else if (names(ds_test_list)[dd] == 'Too_many_NaNs'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Not enough non-NA data to fit a meaningful GAM model.")
          t <- t + 1
        })
      } else if (names(ds_test_list)[dd] == 'Random_NaNs'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_message(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=FALSE,minseglen=45),"NAs in dataset. Consider setting of na.action in GAM call or provide dataset with no missing data.")
          t <- t + 1
        })
      } else if (names(ds_test_list)[dd] == 'Non_Numeric'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Only numeric data is allowed. The following columns of the input data have non-numeric data: U")
          t <- t + 1
        })
      } else if (names(ds_test_list)[dd] == '1_Cov_same'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Some covariates in dataset contain same value for all rows, GAM unable to be fitted in current form.")
          t <- t + 1
        })
      } else if (names(ds_test_list)[dd] == '2_Cov_same'){
        test_that(paste0('Test #',t,':data=',names(ds_test_list)[dd]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Some covariates in dataset contain same value for all rows, GAM unable to be fitted in current form.")
          t <- t + 1
        })
      }

      #If data correct and formula wrong run formula checks.
    } else if ((names(ds_test_list)[dd] == 'Correct_format') & (names(GAM_formula_list)[gg] != 'Correct_formula')){
      if (names(GAM_formula_list)[gg] == 'Wrong_formula'){
        test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Input data.set missing variables required by formula. Please check both formula and dataset.")
          t <- t + 1
        })
      } else if (names(GAM_formula_list)[gg] == 'Not_formula'){
        test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
          expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"GAM formula provided is not correct format please check.")
          t <- t + 1
        })
      }

      #If both correct run other checks.
    } else if ((names(ds_test_list)[dd] == 'Correct_format') & (names(GAM_formula_list)[gg] == 'Correct_formula')){

      #Check to see if code returns correct output with correct formula/dataset and all others settings as default.
      test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
        X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=FALSE,minseglen=45)
        expect_s4_class(X,'GAM_PELT')
        t <- t + 1
      })

      #Loop over minseglen parameters.
      for (mslen in 1:length(minseglen_tests)){

        if (!is.numeric(minseglen_tests[[mslen]])){
          test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
            expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],minseglen=minseglen_tests[[mslen]]),
                         "Minimum segment length must be an integer value.")
            t <- t + 1
          })
        } else {
          if (minseglen_tests[[mslen]]%%1 != 0){
            test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
              expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],minseglen=minseglen_tests[[mslen]]),
                           "Minimum segment cannot be a decimal, please specify as an integer value.")
              t <- t + 1
            })
          } else {
            if (minseglen_tests[[mslen]] < 0){
              test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
                expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],minseglen=minseglen_tests[[mslen]]),
                             "Minimum segment length cannot be negative")
                t <- t + 1
              })
            } else if ((minseglen_tests[[mslen]] > 0) & (minseglen_tests[[mslen]] < 5)){
              test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
                expect_warning(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],minseglen=minseglen_tests[[mslen]]),
                               "Minimum segment length for GAM.PELT is 5, automatically changed to 5.")
                t <- t + 1
              })
            } else if (minseglen_tests[[mslen]] > floor(length(unique(ds_test_list[[dd]]$T))/2)){
              test_that(paste0('Test #',t,':data=',minseglen_tests[[mslen]]), {
                expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],minseglen=minseglen_tests[[mslen]]),
                             "Minimum segment length must be less than half the number of timepoints")
                t <- t + 1
              })
            } else {
              test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
                X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=FALSE,minseglen=minseglen_tests[[mslen]])
                expect_s4_class(X,'GAM_PELT')
                t <- t + 1
              })

              #Loop over verbose, fit parameter and penalties inputs and run tests.
              #These are only checked if correct data and formula are provided as inputs.
              #If not logical report error otherwise run and expect correct format output.
              for (vv in 1:length(verbose_tests)){
                if (verbose_tests[[vv]] == 'NOT LOGICAL'){
                  test_that('Verbose non-logical', {
                    expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=verbose_tests[[vv]]),
                                 "Value of verbose must be a logical and set to TRUE/FALSE.")
                    t <- t + 1
                  })
                } else {
                  test_that(paste0('Test #',t,':data=',verbose_tests[[vv]]), {
                    X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=verbose_tests[[vv]],minseglen=45)
                    expect_s4_class(X,'GAM_PELT')
                    t <- t + 1
                  })
                  for (ff in 1:length(GAM_fit_tests)){
                    if (GAM_fit_tests[[ff]] == 'NOT LOGICAL'){
                      test_that('Fit non-logical', {
                        expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]]),
                                     "Value of fit parameter must be a logical and set to TRUE/FALSE.")
                        t <- t + 1
                      })
                    } else if (GAM_fit_tests[[ff]] == FALSE){
                      test_that('Fit set to TRUE', {
                        expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]]),
                                     "Call to mgcv::gam has fit option set to FALSE. This will not work with GAM.PELT, please set fit=TRUE")
                        t <- t + 1
                      })
                    } else {
                      test_that(paste0('Test #',t,':data=',verbose_tests[[vv]]), {
                        X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]],minseglen=45)
                        expect_s4_class(X,'GAM_PELT')
                        t <- t + 1
                      })
                      #Now check the different penalty types.
                      for (pp in 1:length(penalties_tests)){
                        if (toupper(penalties_tests[[pp]]) %in% c('AIC','BIC','MBIC','SIC','NONE')){
                          test_that('Manual penalty correct', {
                            X <- GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                                          verbose=verbose_tests[[vv]],minseglen=45,
                                          penalty=penalties_tests[[pp]])
                            expect_s4_class(X,'GAM_PELT')
                            t <- t + 1
                          })
                        } else if (toupper(penalties_tests[[pp]]) == 'MANUAL'){
                          checkManualPenalty()
                        } else {
                          test_that('Incorrect penalty', {
                            expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]],
                                                  verbose=verbose_tests[[vv]],fit=GAM_fit_tests[[ff]],
                                                  penalty=penalties_tests[[pp]]),
                                         "Invalid penalty choice - please choose from AIC, BIC/SIC, MBIC, Manual or None. NOTE: If Manual penalty chosen please supply value at pen.value.")
                            t <- t + 1
                          })
                        }
                      }
                    }
                  }
                }
              }

            }
          }
        }
      }

      #If both incorrect throw specific error that both inputs are incorrect.
    } else if ((names(ds_test_list)[dd] == 'Not_dataframe') & (names(GAM_formula_list)[gg] == 'Not_formula')) {
      test_that(paste0('Test #',t,':data=',names(GAM_formula_list)[gg]), {
        expect_error(GAM.PELT(df_in=ds_test_list[[dd]],formula=GAM_formula_list[[gg]]),"Input data not a data.frame and invalid GAM formula provided.")
        t <- t + 1
      })
    }
  }
}
