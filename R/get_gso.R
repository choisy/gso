#' Print GSO data frame
#'
#' Print data frame contains in the gso package. You can use data_frame_summary
#' to know the name of each data frame contains in the package.
#'
#' @details For the moment, only the data frame priority 1 are available
#'
#' @param name name of a data frame contains in the package
#'
#' @return a data frame
#'
#' @examples
#' library(gso)
#'
#' summary <- gso::data_frame_summary
#'
#' # To filter only the name of the data frame priority 1 in the gso package,
#' # to obtain a data frame containing only the name of all the data frame
#' # available for the moment.
#' summary_p1 <- summary[which(summary$priority == 1),]
#'
#' # If you wnat to have the 'Total fertility rate by province'
#' get_gso('Total fertility rate by province')
#'
#' @export
get_gso <- function(name){
  df <- get(name)
  df
}
