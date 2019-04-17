#' Finds keyword
#'
#' Search by keyword in the content data frame and more precisely in the name of
#' the data frame and in the name of the variables and returns a data frame.
#'
#' @param keyword a vector of character string
#'
#' @return a data frame of three variables: \code{data_name}, \code{data_frame}
#' and \code{data}
#'
#' @export
#' @importFrom stats setNames
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
  names_sel1 <- content[row_sel, "data_name"]
  names_sel1 <- unique(unlist(names_sel1))
  # keyword in variable name
  lst_name <- lapply(content$data, colnames)
  lst_name <- setNames(lst_name, content$data)
  names_sel2 <- lapply(lst_name, function(x) {
    any(grepl(paste0(keyword, collapse = "|"), x,
              ignore.case = TRUE))
    })
  names_sel2 <- sapply(names_sel2, function(x) which(x == TRUE))
  names_sel2 <- names(unlist(names_sel2))
  # total results
  name_sel <- unique(c(names_sel1, names_sel2))
  df <- subset(content, data_name %in% name_sel,
                c(data_name, data_frame, data))
  df
}

## quiets concerns of R CMD check for the values that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("content", "data",
                                                        "data_name",
                                                        "data_frame"))
