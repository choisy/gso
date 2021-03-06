#' Extract data frame from content
#'
#' Extracts a data frame or a list of data frame from the name `data_name` of
#' the data frame selected in the content data frame.
#'
#' @param keyword a vector of character string, of value from the column
#'     `data_name` of the content data frame
#'
#' @return a data frame or a list of data frame
#'
#' @export
#'
#' @examples
#' # for one keyword
#' pop <- extract_df("demography_5")
#'
#' # for multiple keyword
#' lst_demo <- extract_df(c("demography_5", "demography_3"))
extract_df <- function(keyword) {

  content <- mget("content", inherits = TRUE)[[1]]

  if (all(keyword %in% content$data_name) == FALSE) {
    stop(
      paste0("The argument `keyword` should contain character vector of value,",
             ", contained in the 'data_name' column of 'content' data frame."))
  }

  lst <- lapply(keyword, function(x) {
    df <- subset(content, data_name == x, "data")
    df <- unlist(df, FALSE)
  })
  if (length(lst) == 1) {
    lst <- lst[[1]][[1]]
    }
  lst
}
