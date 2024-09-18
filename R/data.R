#' Get Stock Data from Yahoo Finance
#'
#' @param ticker
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
get_yahoo <- function(ticker, start = 0, end = Sys.Date()) {
  tidyquant::tq_get(ticker, from = start, to = end)
}

get_fmp <- function(ticker, start = 0, end = Sys.Date(), api_key) {
  out <- jsonlite::read_json(paste0("https://financialmodelingprep.com/api/v3/historical-price-full/",
                                    ticker, "?apikey=", api_key), simplifyVector = TRUE)

  out$historical |>
    dplyr::as_tibble() |>
    dplyr::mutate(date = lubridate::ymd(date))
}

#' Get Company Profile from Financial Modeling Prep
#'
#' @param api_key
#' @param temp_dir
#' @param time_out
#'
#' @return
#' @export
#'
#' @examples
get_profiles <- function(api_key, temp_dir = tempdir(), time_out = 300) {
  url <- paste0("https://financialmodelingprep.com/api/v4/profile/all?apikey=", api_key)
  file <- tempfile(fileext = ".csv", tmpdir = temp_dir)

  # timeout options
  old_timeout <- getOption("timeout")  # Save the current timeout
  on.exit(options(timeout = old_timeout))  # Ensure the timeout is reset when the function exits
  options(timeout = time_out)  # Set the new timeout

  download.file(url, file)
  readr::read_csv(file)
}

#' Get Financial Statements from Financial Modeling Prep
#'
#' @param ticker
#' @param api_key
#' @param report
#' @param period
#'
#' @return
#' @export
#'
#' @examples
get_fmp_fin <- function(ticker, api_key, report = "is", period = "annual") {
  if (stringr::str_to_lower(stringr::str_remove_all(report, "\\.|,|-|_")) %in% c("is", "income", "incomestatement")) {
    url <- paste0("https://financialmodelingprep.com/api/v3/financials/income-statement/",
                  ticker, "?period=", period, "&apikey=", api_key)
    out <- jsonlite::read_json(url, simplifyVector = TRUE)
  } else if (stringr::str_to_lower(stringr::str_remove_all(report, "\\.|,|-|_")) %in% c("bs", "balancesheet", "balance")) {
    url <- paste0("https://financialmodelingprep.com/api/v3/financials/balance-sheet-statement/",
                  ticker, "?period=", period, "&apikey=", api_key)
    out <- jsonlite::read_json(url, simplifyVector = TRUE)
  } else if (stringr::str_to_lower(stringr::str_remove_all(report, "\\.|,|-|_")) %in% c("cf", "cashflow", "cash")) {
    url <- paste0("https://financialmodelingprep.com/api/v3/financials/cash-flow-statement/",
                  ticker, "?period=", period, "&apikey=", api_key)
    out <- jsonlite::read_json(url, simplifyVector = TRUE)
  } else {
    stop("Invalid report type. Please use 'is', 'bs', or 'cf'.")
  }

  out |> dplyr::as_tibble() |> tidyr::unnest(financials)
}
