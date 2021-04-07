
#' Print"fars_read function"
#' 
#' This function for reading existing files. You only need to input a filename
#'       with a full path, it would be read if it exists. If filename does not exist, would 
#'       get an error "file filename does not exist"
#'       
#' @param filename is a character string for file's name with full path that the function would read it.
#'
#' @return This function returns a tbl_df from input data.
#'
#' @importFrom ("readr", "read_csv")
#' @importFrom ("dplyr", "tbl_df")
#'
#' @examples 
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Print"make_filename function"
#' 
#' This function takes the argument of the year and prints out 
#'      the formatted file name.The argument year should to be a integer,
#'      or an object that can coerce to a integer, if not would get an error.
#'
#' @param year, is an integer or an object that can coerce to integer, 
#'    that would be appended to a formatted filename when printed.
#' 
#' @return a formatted filename.
#' 
#' @examples 
#' make_filename(2013)
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Print"fars_read_years function"
#' 
#' This function use to make up a list of filenames from the make_filename function 
#'   above. The function would give a warning and return NULL if argument year invalid.
#'   
#' @param years, is a list of years
#' 
#' @return this function returns a list of formatted filenames.
#' 
#' @importFrom ("dplyr", "mutate", "select")
#' 
#' @examples 
#' fars_read_years(list("2013", "2014", "2015"))
#' 
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Print"fars_summarize_years function"
#' 
#' This function will concatenate by rows all tbl_dfs from filenames that are made up of the fars_read_years function.
#'   Then finally, count by year.
#'   
#' @param years, is a list of years.
#' 
#' @return This function returns a tbl_df includes columns by years and a row for the counted number.
#' 
#' @importFrom ("dplyr", "bind_rows", "group_by", "summarize")
#' @importFrom ("tidyr", "spread")
#' 
#' @examples 
#' fars_summarize_years(list("2013", "2014", "2015"))
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Print"fars_map_state function"
#' 
#' This function plots by year and STATE's number from data of tbl_df that
#'  made up from read a formatted filename by year.
#'  
#'  @param There are two arguments. The first is state.num, is STATE's number. 
#'    The second argument is year, is number of year for make up filename.
#'    If state.num is not in STATE's number, the function gives an error "invalid STATE number...".
#'    If no data, the function gives a message "no accident to plot".
#'    
#'  @return This function returns a map of the places where the crash occurred.
#' 
#'  @importFrom ("dplyr", "filter") 
#'  @importFrom ("maps", "map")
#'  @importFrom ("graphics", "points")
#'  
#'  @examples 
#'  fars_map_state(5, 2013)
#'  fars_map_state(3, 2014)
#'  
#'  @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}