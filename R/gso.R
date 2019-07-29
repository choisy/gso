#' gso : Data from the General Statistical Office (GSO) of Vietnam
#'
#' This package contains the data from the General Statistical Office of Vietnam
#' (last update, September 21st, 2018) ordered in one nested data frame named
#' "content" which contains the data frame and some complementary information
#' (spatail & temporal definition ...)
#'
#' @docType package
#' @name gso
NULL

## quiets concerns of R CMD check for the values that appear in base::subset()
if (getRversion() >= "2.15.1") utils::globalVariables("data_name")
