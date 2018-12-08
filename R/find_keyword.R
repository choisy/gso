#' Finds keyword
#'
#' Search by keyword in the content data frame and more precisely in the name of
#' the data frame and in the name of the variables and returns a data frame.
#'
#' @param keyword a vector of character string
#'
#' @return a data frame of three variables: `data_name`, `data_frame` and `data`
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map keep
#' @importFrom dplyr filter select
#' @export
#'
#' @examples
#' # for one keyword
#' pov <- find_keyword("poverty")
#'
#' # for multiple keyword
#' export_import <- find_keyword(c("exportation", "importation"))
find_keyword <- function(keyword) {
  # keyword in data frame name
  row_sel <- grep(paste0(keyword, collapse = "|"), content$data_frame,
                  ignore.case = TRUE)
  names_sel1 <- content[row_sel, "data_name"] %>% unlist %>% unique()
  # keyword in variable name
  lst_name <- content$data %>% purrr::map(names)
  names_sel2 <- lapply(lst_name, function(x) {
    any(grepl(paste0(keyword, collapse = "|"), x,
              ignore.case = TRUE))
    }) %>%
    purrr::keep(isTRUE) %>% names()
  # total results
  name_sel <- c(names_sel1, names_sel2) %>% unique
  df <- content %>% filter(data_name %in% name_sel) %>%
    select(data_name, data_frame, data)
  df
}
