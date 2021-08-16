#' @description The fars_read function wraps the read_csv function.
#' This function takes a file in csv format reads it and returns
#' a tibble data frame if the file exists. Otherwise the error
#' message filename does not exist will be displayed.
#'
#' @param filename Name of the csv file as a string.
#' @return tibble data frame
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv function
#' @importFrom dplyr tbl_df function
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @description The function make_filename creates a filename
#' with the given year
#' make_filename takes string year as input and appends that year to the
#' filename
#' @param year as a string.
#' @return file name with the year attached
#' @examples
#' make_filename("2013")
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' @description The function fars_read_years list the fatality data for each
#' month in the years given
#' fars_read_years function takes vector of years as input
#' and selects the MONTH and year columns and lists tibble data
#' frame for each year
#' @param vector of years
#'
#' @return lists of tibble dataframes selecting the fatal accident
#' data for each month in the given years. If the year is not matching
#' it gives the warning 'invalid year'
#'
#' @examples
#' fars_read_years(years = c(2013, 2014, 2015))
#'
#' @importFrom mutate and select from dplyr package
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

#' @description The fars_summarize_years function gives the total number
#' of fatalities for each month of given years as input
#' fars_summarize_years summarises the fatalities each month for
#' all the given years
#' @param vector of years
#' @return the summary of fatalities by month and year
#' @example
#' fars_summarize_years <- function(years = c(2013, 2014, 2015))
#'
#' @importFrom bind  group_by summarize from dplyr package
#' @importFrom spread from tidyr package
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @description The fars_map_state function maps the fatality of the state
#' for a given year
#' fars_map_state function takes the state number and year as input
#' and maps the fatality data based on the location
#' @param state number and year
#' @return plot of fatalities for the given state and year
#' if the state number is invalid it gives error message
#' "invalid STATE number: "
#'
#'  @example
#'   fars_map_state(25,2013)
#'
#' @importFrom filter from dplyr package
#' @importFrom map from maps package
#' @importFrom points from braphics package
#'
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
