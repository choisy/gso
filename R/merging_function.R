# packages neeeded
library(magrittr)
library(dplyr)
library(tidyr)

# Prerequisite -----------------------------------------------------------------
splits <- readRDS("data-raw/splits.RDS")
ah_splits <- readRDS("data-raw/ah_splits.RDS")
load(file = "data/pop_size.rda")
load(file = "data/data_frame_summary.rda")
load(file = "R/sysdata.rda")

# Functions GENERIC ------------------------------------------------------------
#' Filters dataframe by a time range
#'
#' Filters a data frame to keep only the data corresponding to a certain time
#' range (between \code{to} and \code{from} (exclude)).
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A data frame with the same variables as \code{df}.
#' @keywords internal
#' @noRd
select_date <- function(df, from, to) {
  df %<>% mutate(date = as.Date(paste0(year, 01, 01, sep = "-"))) %>%
    filter(date >= from, date <= to) %>%
    select(-date)
}


################################################################################
#' Filters and order splits events by a time range
#'
#' Filters a list of splits event to keep only the event corresponding to a
#' certain time range (between \code{to} and \code{from} (exclude)) and order
#' them from the older to the more recent
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @return A list with the same variables as \code{splits_lst}.
#' @keywords internal
#' @noRd
select_events <- function(splits_lst, from, to) {
  sel <- purrr::map(splits_lst, 3) > as.Date(from) &
    purrr::map(splits_lst, 3) <= as.Date(to)
  lst <- splits_lst[sel] %>% purrr::sort_by(3)
}

################################################################################
#' Vectorises provinces by event
#'
#' Selects the name of the provinces concerned by one or multiple splits events
#' and returns a list with a vector for each event and containing all the
#' province names concerned by this event.
#'
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @return A list of vector, each vector corresponds to one event and contains
#' the province names concerned by this event.
#' @keywords internal
#' @noRd
province_splits <- function(lst_split) {
  provinces <- lapply(names(lst_split), function(x) {
    combined <- purrr::map(lst_split[x], 1) %>% unlist() %>% as.vector()
    elements <- purrr::map(lst_split[x], 2) %>% unlist() %>% as.vector()
    province <- c(combined, elements)
  }) %>% setNames(names(lst_split))
  provinces
}

################################################################################
#' Prepares dataframe
#'
#' Applies for one event on a data frame previously filter to keep only the data
#' of the \code{province} linked to the event.
#' Prepares a data frame who joins all the data together by \code{year} without
#' keeping the \code{province} information.
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @return A data frame with the variables:\code{year}, \code{month},
#' \code{incidence} and \code{mortality}
#' @keywo
prepare_data <- function(df) {
  df <- split(df, as.character(unique(df$province))) %>%
    purrr::reduce(full_join, by = names(df))
}

################################################################################
#' Does one merging event
#'
#' Applies for one event to a data frame previously filter to keep only the data
#' of the \code{province} linked to the event.
#' Merges multiple provinces together by applying a mathematical function on the
#' data.
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param FUN A function to apply on the data when merging the province together
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
gather_sum <- function(df, FUN, df2, args, FUN2){

  # prepare the arguments in a good format
  args2 <- c("value", unlist(args))
  targs_quoted <-  do.call(call, c("list", lapply(args2, as.name)), quote=TRUE)

  # Prepare the df in a good format and group the data by year and key for the
  # merging event
    df %<>%
      gather(name, value, contains("value")) %>%
      select(-matches("name")) %>%
      group_by(year, key)

  # if two dfs and the parameter args were provided, apply the merging event on
  # the both of them (with the possibility to apply two different function on
  #  them), if it's not the case, the merging event will be apply only
  # on the first df.
  if(is.data.frame(df2) & is.null(args) == FALSE){
    args_quoted <- do.call(call, c("list", lapply(args, as.name)), quote=TRUE)
    df %<>%
      summarise_(
        value = lazyeval::interp(~do.call(FUN, xs),
          .values = list(FUN = FUN, xs = targs_quoted)),
        args = lazyeval::interp(~do.call(FUN2, args),
           .values = list(FUN2 = FUN2, args = args_quoted))) %>%
      rename_(.dots = setNames(list("args"), args))
  } else {
    df %<>% summarise_(value = lazyeval::interp(
        ~do.call(FUN, xs),
        .values = list(FUN = FUN, xs = targs_quoted)))
  }
  return(df)
}

################################################################################
#' Merging Ha Noi / Ha Son Binh event
#'
#' Applies only if the time range contains the split and the combine event of
#' Ha Noi & Ha Son Binh, does an additional merging on Hanoi and Ha Son Dinh.
#'
#' @param df A  data frame should contain at least the variables \code{year},
#'  \code{province} containing \code{"Ha Noi"} and \code{"Ha Son Binh"}
#' @param FUN A function to apply on the data when merging the province together
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process
#' @return A data frame with the same variables as \code{df}
#' @keywords internal
#' @noRd
hanoi_function <- function(df, FUN, df2, args, FUN2) {
  tab <- split(df, df$province %in% c("Ha Noi", "Ha Son Binh"))
  tab$`TRUE` %<>%
    prepare_data %>%
    gather_sum(FUN = FUN, df2 = df2, args = args, FUN2 = FUN2) %>%
    mutate(province = "Ha Noi")
  bind_rows(tab$`TRUE`, tab$`FALSE`)
}

################################################################################
#' Merges provinces
#'
#' Merges data accordingly to a time range and by the provinces
#' concerned by a split/combined event and return a data frame for the time
#' range imputed.
#'
#' @param df A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param FUN A function to apply on the data when merging the province together
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined}, a slot \code{elements} and a slot \code{date}. The first one
#' contains the name of the merged provinces and the second one contains a
#' vector of the names of the provinces to merge.
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range) are merged.
#' @keywords internal
#' @noRd
merge_province <- function(df, FUN, from, to, splits_lst,
                           df2, args, FUN2)
{
  # select the list of event corresponding at the time range from - to
  lst_events <- select_events(splits_lst, from = from, to = to)

  # if the list contains some events, merge or split the province concerned
  if (length(lst_events) > 0) {

    for (i in rev(seq_along(lst_events))) {
      province_lst <- province_splits(lst_events[i])
      tmp <- split(df, df$province %in% province_lst[[1]])

      if (tmp$`TRUE` %>% length > 0){

        # Take care of the problem of NA for province before year of creation,
        # some dataset have some for some provinces before their year of
        # creation
        if (anyNA(tmp$`TRUE`) == TRUE &
            sum(!province_lst[[1]] %in% "Ha Tay")
            /length(province_lst[[1]]) == 1){
          # Add the data of the province existing before the split event to the
          # data frame containing the other province none of interest at this
          # point and filter the data  to keep only the split event data and re-
          # merge them together
          limit <- lst_events[i][[1]]$date %>% lubridate::year(.)
          add_df <- tmp$`TRUE` %>%
            dplyr::filter(year < limit) %>%
            filter(province == lst_events[i][[1]]$combined)
          tmp$`FALSE` %<>% rbind(add_df)
          tmp$`TRUE` %<>% dplyr::filter(year >= limit)
        }

        # As Ha Tay is a merging event with Hanoi in 2008, another process is
        # province to take care of the problem of the presence of NA for Ha Tay
        # after 2008
        if (anyNA(tmp$`TRUE`) == TRUE &
            sum(!province_lst[[1]] %in% "Ha Tay")
            /length(province_lst[[1]]) != 1){
          limit <- lst_events[i][[1]]$date %>% lubridate::year(.)
          add_df <- tmp$`TRUE` %>%
            dplyr::filter(province != "Ha Tay" & year >= limit)
          tmp$`FALSE` %<>% rbind(add_df)
          tmp$`TRUE` %<>%
            dplyr::filter(year < limit)
        }

        # Merge back together the province
        if (tmp$`TRUE` %>% dim %>% .[1] > 0){
          tmp$`TRUE` %<>%
            prepare_data %>%
            gather_sum(FUN, df2 = df2, args = args, FUN2 = FUN2) %>%
            mutate(province = names(province_lst[1]))
          df <- bind_rows(tmp$`TRUE`, tmp$`FALSE`)

        } else {df <- tmp$`FALSE`}
      } else {df <- tmp$`FALSE`}
    }
  } else {df}

  # if the time range contains the split and the combine event of
  # Ha Noi & Ha Son Binh, does an additional merging on Hanoi and Ha Son Dinh.
  if (from < 1992 & to > 2008){
    df %<>% hanoi_function(FUN, df2 = df2, args = args, FUN2 = FUN2)
  }

  return(df)
}


################################################################################
#' Merges provinces
#'
#' Tidy the data and merges data accordingly to a time range and by the
#' provinces concerned by a split/combined event and return a data frame for the
#'  time range imputed.
#'
#' @param df  A data frame containing at least the variables \code{province},
#' \code{year}.
#' @param sel A vector of character to select only the variable to merge. Can
#' be useful if, to merge different variable, you need to use different
#' mathematical operation. By default, select all the variables.
#' @param FUN A function to apply on the data when merging the province
#' together.
#' @param diseases A vector of character used to know which history of vietnam
#' has to be taken for the merging event as two diseases has different story.
#' Used if you want the same merging event as your infectious disease dataframe,
#'  for more details look at the /code{gdpm package}. By default, \code{NULL}
#' @param from Initial date of the time range, of the class \code{Date}.
#' @param to Final date of the data, of the class \code{Date}.
#' @param splits_lst A list containing a list of event, each code with a slot
#' \code{combined} and a slot \code{elements}. The first one contains the name
#' of the merged provinces and the second one contains a vector of the names of
#' the provinces to merge.
#' @param df2 A data frame containing at least the variables \code{province},
#' \code{year}. Can be used to provide additional arguments through the
#' paramaters args by providing the name of the column(s) containing the data.
#' @param args A vector or list of additional arguments to pass to FUN
#' @param FUN2 A function to apply on df2 when merging the province together,
#' to keep the same information at every step of the merging process. By
#' default \code{sum}
#' @return A object of the same class as \code{df} in which all the provinces
#' that needed to be merged (according to the time range) are merged.
#' @keywords internal
#' @noRd
spread_merge_province <- function(df, sel = names(df), FUN, diseases = NULL,
                                  from , to, df2 = NULL, args = NULL,
                                  FUN2 = sum){
  # test df2 format, should be a data frame
  if (is.null(df2) == FALSE){
    if(is.data.frame(df2) == FALSE){
      stop(paste0("the parameter df2 is not in a good format: ",class(df2),
                  ", df2 should be a data frame."))
    }
  }
  # select the history of merging/spliting event depending on the parameters
  # diseases
  if (grep(paste(diseases, collapse = "|"), "hepatitis|amoebiasis") %>%
      length() != 0){
    spl <- ah_splits
  } else {
    spl <- splits
  }

  # If the df contain at least a column "province" and a column "year",
  # gather and merge the data accordingly with the time serie implemented and
  # the history of Vietnam selected.
  if (grep("province|year", names(df)) %>% length >= 2){
    # Join df2
    if (is.data.frame(df2) == TRUE){
      sel2 <- grep(names(df) %>% paste(collapse = "|"), names(df2), value = T)
      df <- suppressWarnings(left_join(df,df2, by = sel2))
      sel <- c(sel, args)
    }

    sel <- c("province", "year", sel)
    gather_sel <-  c("province", "year")
    if(is.null(args) == FALSE){
      gather_sel <- c("province", "year", args)
    }
    df %<>%
      select(one_of(sel)) %>%
      gather(key, value, -one_of(gather_sel)) %>%
      mutate(year = as.integer(year)) %>%
      merge_province(FUN, from = from, to = to,splits_lst = spl, df2 = df2,
                     args = args, FUN2 = FUN2) %>%
      ungroup %>%
      arrange(province, year) %>%
      select(province, year, key, value)
  }
  # If the df doesn't contain the two necessary columns, print a warning message
  else {
    df
    warning("If you want to merge the province back together accordingly to
Vietnam history, the data frame should contain at least the variables province,
year")
  }
  df
  return(df)
}



## error time selection
## Add the two splits list in sysdata.rda
## Assert for the test if


# Functions GSO ----------------------------------------------------------------
# function to get the dataframe
#df <-  get(vect)

# load data --------------------------------------------------------------------
pop_size  %<>% select(province,year,total)

p_list <- data_frame_summary %>%
  filter(`priority` == "1") %>%
  filter(`spatial resolution` == "province") %>%
  select(`data frame`) %>%
  unlist %>%
  as.vector()

#from = "1990-01-01"
#to = "2010-12-31"
#splits_lst = splits_list
#sel = names(df)

df <- get(p_list[100]) %>%
  spread_merge_province(sel = c("Others", "Agriculture_forestry_fishery"),
                        FUN = sum,
                        from = "1992-01-01", to = "2010-12-31")

df <- get(p_list[100]) %>%
  spread_merge_province(FUN = sum,
                        from = "1992-01-01", to = "2010-12-31",
                        df2 = pop_size, args = NULL)

test <- get(p_list[88])
df <- get(p_list[88]) %>%
  spread_merge_province(FUN = weighted.mean,
                        from = "1992-01-01", to = "2010-12-31",
                        df2 = pop_size, args = "total")

lapply(seq_along(p_list), function(x){
  df <- get(p_list[x]) %>%
    spread_merge_province(FUN = sum,
                          from = "1992-01-01", to = "2010-12-31")
})


lapply(seq_along(p_list), function(x){
  df <- get(p_list[x]) %>%
    spread_merge_province(FUN = weighted.mean,
                          from = "1992-01-01", to = "2010-12-31",
                          df2 = pop_size, args = "total")
})

