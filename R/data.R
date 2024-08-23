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
#'   ...
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
#'   ...
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
#'   ...
#' }
#' @source <https://uk-air.defra.gov.uk/data/data_selector>
"AURN_PM"
