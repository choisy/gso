#' Vietnamese population sizes
#'
#'
#' A dataset containing the Vietnamese population sizes by province (64) and
#' year from 1995 to 2015 expressed in thousand peoples.
#'
#'
#' The precision is 100 people.
#'
#'
#' @format A data frame with 1344 rows and 7 variables:
#' \itemize{
#'    \item \code{year}: year
#'    \item \code{province}: province name
#'    \item \code{total}: total population
#'    \item \code{male}: male population
#'    \item \code{female}: female population
#'    \item \code{urban}: urban population
#'    \item \code{rural}: rural population
#' }
#'
#' @source General Statistical Office of Vietnam (\url{http://gso.gov.vn/Default_en.aspx?tabid=491}).
#'
"pop_size"


#' Summary Table
#'
#'
#' A dataset containing the description of all the data frame contain in the
#' package.
#'
#'
#' The dataset contains, the names of each data frame,the folder corresponding
#' which is also the category, a sub category for more information, the
#' time-range and time-resulation of the data, the spatial resolution of the
#' data and the priority. \cr\cr
#' This table can be use as a resume of all the other data_frame contain in this
#'  package and maybe be useful for computing on the various files.
#'
#'
#' @details For each data frame a code of priority has been defined, either 0
#' (equal to NA) or 1. The 113 data frame quoted priority 1 are the only one
#' available in a clean and tidy version through the package for the moment.
#' Priority 1 contains the 107 data frames containing data expressed by province
#'  and 6 data frame containing important information for the study of
#'  epidemiology:
#' \itemize{
#'     \item \code{'Land use (As of 1 January 2014)'}
#'     \item \code{'Average population by sex and by residence'}
#'     \item \code{ 'Sex ratio by residence'}
#'     \item \code{'Crude birth rate, crude death rate and natural increase rate
#'      of population by residence'}
#'     \item \code{'Total fertility rate by residence'}
#'     \item \code{'Child mortality rate by sex and by residence'}
#'}
#'
#'
#' @usage data(data_frame_summary)
#'
#'
#' @format A data frame of 316 rows and 8 variables:
#' \itemize{
#'    \item \code{category}: theme of the data frame. All the data are separated
#'    in 10 category: "Administrative Unit, Land and Climate",
#'    "Population and Employment", "National Accounts",
#'    "Investment and Construction", "Agriculture, Forestry and Fishery",
#'    "Industry", "Trade, Price and Tourism",
#'    "Transport, Postal Services and Telecommunications", "Education"
#'    and "Health, Culture and Living Standard".
#'    \item \code{subcategory}: subcategory of the data frame
#'    \item \code{time range}: time range expressed in year
#'    \item \code{time resolution}: unit of time used in the data frame (year,
#'    month or single time point)
#'    \item \code{spatial resoluation}: spatial unit used in the data frame
#'    (province, stations, city, rivers ...)
#'    \item \code{data frame}: name of the data frames
#'    \item \code{data_name}: name of the data frames in the data folder
#'    \item \code{priority}: priority number (0, 1)
#' }
#'
#'
#' @source General Statistical Office of Vietnam (\url{http://gso.gov.vn/Default_en.aspx?tabid=491}).
#'
"data_frame_summary"
