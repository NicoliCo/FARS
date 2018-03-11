#' Read FARS dataset
#'
#' This function takes as input a data set from the US National Highway Traffis Safety
#' Administration's
#' [Fatality Anlaysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars)
#' (FARS) and loads it into a data frame
#'
#'
#' @param filename Either a path to a file, a connection, or literal data
#' (either a single string or a raw vector). See [readr::read_csv()]
#'
#' @return If succesful, returns a tibble with the contents of the file. If the file
#' doesn't exist, an error message is displayed
#'
#' @examples
#' fars_read(filename = 'accident_2013.csv.bz2')
#'
fars_read <- function(filename) {
        filename <- system.file("extdata", filename, package = "FARS")
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a FARS Filename from Year
#'
#' This function takes as input a year and creates a filename in the format of
#' data sets from the US National Highway Traffis Safety
#' Administration's
#' [Fatality Anlaysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars)
#' (FARS).
#'
#'
#' @param year Numeric. Will be coerced to an integer with [as.integer()]
#'
#' @return A string in the format "accident_XXXX.csv.bz2" where XXXX is the year
#'
#' @examples
#' make_filename(2013)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads FARS data for Years
#'
#' This function takes as input a list of years and loads the data from
#' US National Highway Traffis Safety
#' Administration's
#' [Fatality Anlaysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars)
#' (FARS) for those years.
#'
#'
#' @param years List of years. Each year will be coerced to an integer with [as.integer()]
#'
#' @return A list of tibbles, one for earch year and containing only the Month and Year from each row of
#' the FARS data for the given years. If a year is invalid, then a warning message is issued
#' and NULL is returned.
#'
#' @examples
#' fars_read_years(c(2013,2014,2015))
#'
#' @importFrom magrittr "%>%"
#'
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

#' Summarize FARS Data for Years
#'
#' This function takes as input a list of years and returns a summary of the data from
#' US National Highway Traffis Safety
#' Administration's
#' [Fatality Anlaysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars)
#' (FARS) for those years.
#'
#' @param years List of years. Each year will be coerced to an integer with [as.integer()]
#'
#' @return A tibble containing the number of entries in the FARS data for each month of the given years. First column
#' is the month, and subsequent columns for each year. If a year is
#' invalid, then a warning message is issued.
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#'
#' @importFrom magrittr "%>%"
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map FARS Entries for a State and Year
#'
#' This function takes as input a state and a year and plots a map of
#' the location of the entries in the US National Highway Traffis Safety
#' Administration's
#' [Fatality Anlaysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars)
#' (FARS) for the given state and year. The function throws an error if the state/year
#' combination do not exist in the FARS database.
#'
#' @param state.num Numeric indicating the state for the map. Will be coerced to an integer.
#' @param year Numeric indicating the year to map. Will be coerced to an integer.
#'
#' @return As a side-effect, plots a map of FARS entries for the state and year
#'
#' @examples
#' fars_map_state(1,2013)
#'
#'
#' @export
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
