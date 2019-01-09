#' Extract data frame from content
#'
#' Extracts a data frame or a list of data frame in the content data frame.
#'
#' @param keyword a vector of character string
#'
#' @return a data frame or a list of data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @export
#'
#' @examples
#' # for one keyword
#' pop <- extract_df("demography_5")
#'
#' # for multiple keyword
#' lst_demo <- extract_df(c("demography_5", "demography_3"))
extract_df <- function(keyword) {
  lst <- lapply(keyword, function(x) {
    df <- content %>%
      filter(data_name == x) %>%
      select(data) %>%
      unlist(FALSE)
  })
  if (length(lst) == 1) {lst <- lst[[1]][[1]]}
  lst
}
