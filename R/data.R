#' Automatic Urban and Rural Network (AURN) NO<sub>2</sub> data.
#'
#' @description
#' This dataset contains daily aggregated air quality data from the United Kingdom's Automatic Urban
#' and Rural Network (AURN) for the primary air pollutant nitrogen dioxide (NO<sub>2</sub>).Data is provided for
#' 74 sites around the UK that provided a complete daily mean dataset (with no missing data) for the time period
#' 1<sup>st</sup> February to 31<sup>st</sup> August 2020. The data is already formatted into a data frame for
#' running with GAM-PELT.
#'
#' A description of the AURN network is available from the [UK-AIR](https://uk-air.defra.gov.uk/networks/network-info?view=aurn)
#' website.The data are freely available to download and are provided by the Department for the Environment and Rural Affairs
#' (Defra) under the following attribution statement:
#'
#' (C) Crown copyright 2021 Defra via uk-air.defra.gov.uk, licensed under the [Open Government Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/2/]).
#'
#'
#' @format ## `AURN_NO2`
#' A data frame with 15762 rows and 9 columns:
#' \describe{
#'   \item{U}{Longitude of the station location (in -180 to 180.0 format)}
#'   \item{V}{Latitude of the station location (in -90.0 to 90.0 format)}
#'   \item{T}{Time index of measurement}
#'   \item{tstamp}{Time stamp of measurement (in YYY-MM-DD format)}
#'   \item{SID}{Site ID of measurement location}
#'   \item{SCODE}{Site Code}
#'   \item{SFNAME}{Site Name}
#'   \item{STYPE}{Site type - will be one of Urban Background, Rural Background, Urban Traffic, Urban Industrial, Suburban Industrial or Suburban Background}
#'   \item{Poll_Value}{Daily concentration of pollutant NO<sub>2</sub> in \eqn{\mu} gm<sup>-3</sup>}
#' }
#' @source <https://uk-air.defra.gov.uk/data/data_selector>
"AURN_NO2"

#' Automatic Urban and Rural Network (AURN) O<sub>3</sub> data.
#'
#' @description
#' This dataset contains daily aggregated air quality data from the United Kingdom's Automatic Urban
#' and Rural Network (AURN) for the secondary air pollutant ozone (O<sub>3</sub>).Data is provided for
#' 30 sites around the UK that provided a complete daily mean dataset (with no missing data) for the time period
#' 1<sup>st</sup> February to 31<sup>st</sup> August 2020. The data is already formatted into a data frame for
#' running with GAM-PELT.
#'
#' A description of the AURN network is available from the [UK-AIR](https://uk-air.defra.gov.uk/networks/network-info?view=aurn)
#' website.The data are freely available to download and are provided by the Department for the Environment and Rural Affairs
#' (Defra) under the following attribution statement:
#'
#' (C) Crown copyright 2021 Defra via uk-air.defra.gov.uk, licensed under the [Open Government Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/2/]).
#'
#'
#' @format ## `AURN_O3`
#' A data frame with 6390 rows and 9 columns:
#' \describe{
#'   \item{U}{Longitude of the station location (in -180 to 180.0 format)}
#'   \item{V}{Latitude of the station location (in -90.0 to 90.0 format)}
#'   \item{T}{Time index of measurement}
#'   \item{tstamp}{Time stamp of measurement (in YYY-MM-DD format)}
#'   \item{SID}{Site ID of measurement location}
#'   \item{SCODE}{Site Code}
#'   \item{SFNAME}{Site Name}
#'   \item{STYPE}{Site type - will be one of Urban Background, Rural Background, Urban Traffic, Urban Industrial, Suburban Industrial or Suburban Background}
#'   \item{Poll_Value}{Daily concentration of pollutant O<sub>3</sub> in \eqn{\mu} gm<sup>-3</sup>}
#' }
#' @source <https://uk-air.defra.gov.uk/data/data_selector>
"AURN_O3"

#' Automatic Urban and Rural Network (AURN) PM<sub>2.5</sub> data.
#'
#' @description
#' This dataset contains daily aggregated air quality data from the United Kingdom's Automatic Urban
#' and Rural Network (AURN) for the primary air pollutant particulate matter of diameter less than 2.5 micrometres (PM<sub>2.5</sub>).Data is provided for
#' 30 sites around the UK that provided a complete daily mean dataset (with no missing data) for the time period
#' 1<sup>st</sup> February to 31<sup>st</sup> August 2020. The data is already formatted into a data frame for
#' running with GAM-PELT.
#'
#' A description of the AURN network is available from the [UK-AIR](https://uk-air.defra.gov.uk/networks/network-info?view=aurn)
#' website.The data are freely available to download and are provided by the Department for the Environment and Rural Affairs
#' (Defra) under the following attribution statement:
#'
#' (C) Crown copyright 2021 Defra via uk-air.defra.gov.uk, licensed under the [Open Government Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/2/]).
#'
#'
#' @format ## `AURN_PM`
#' A data frame with 6390 rows and 9 columns:
#' \describe{
#'   \item{U}{Longitude of the station location (in -180 to 180.0 format)}
#'   \item{V}{Latitude of the station location (in -90.0 to 90.0 format)}
#'   \item{T}{Time index of measurement}
#'   \item{tstamp}{Time stamp of measurement (in YYY-MM-DD format)}
#'   \item{SID}{Site ID of measurement location}
#'   \item{SCODE}{Site Code}
#'   \item{SFNAME}{Site Name}
#'   \item{STYPE}{Site type - will be one of Urban Background, Rural Background, Urban Traffic, Urban Industrial, Suburban Industrial or Suburban Background}
#'   \item{Poll_Value}{Daily concentration of pollutant PM<sub>2.5</sub> in \eqn{\mu} gm<sup>-3</sup>}
#' }
#' @source <https://uk-air.defra.gov.uk/data/data_selector>
"AURN_PM"

#' Random Seeds for simulation studies.
#'
#' @description
#' Random seeds required to reproduce the datasets used in the simulation studies for the GAM-PELT paper.
#' 100 replicates were run for each scenario in batches of 10. E.g. To reproduce Scenario 4b replicate 4
#' extract the random seed from the following location in the data frame (SCEN='4b', BATCH=1, Replicate=Replicate_4).
#' Or to reproduce Scenario 1a replicate 24 extract the random seed as follows (SCEN='1a', BATCH=3, Replicate=Replicate_4).
#'
#' @format ## 'SimStudies_rseeds'
#' A data frame with 170 rows and 12 columns:
#' \describe{
#'   \item{SCEN}{Scenario}
#'   \item{BATCH}{Batch}
#'   \item{Replicate_1}{Random seed for 1st replicate in batch XX}
#'   \item{Replicate_2}{Random seed for 2nd replicate in batch XX}
#'   \item{Replicate_3}{Random seed for 3rd replicate in batch XX}
#'   \item{Replicate_4}{Random seed for 4th replicate in batch XX}
#'   \item{Replicate_5}{Random seed for 5th replicate in batch XX}
#'   \item{Replicate_6}{Random seed for 6th replicate in batch XX}
#'   \item{Replicate_7}{Random seed for 7th replicate in batch XX}
#'   \item{Replicate_8}{Random seed for 8th replicate in batch XX}
#'   \item{Replicate_9}{Random seed for 9th replicate in batch XX}
#'   \item{Replicate_10}{Random seed for 10th replicate in batch XX}
#' }
#' @source This dataset was generated as part of the simulation studies to allow reproducibility of the simulations.
"SimStudies_rseeds"
