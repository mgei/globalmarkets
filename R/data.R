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

#' Get Economic Data from FRED
#'
#' @param series
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
get_fred <- function(series, start = 0, end = Sys.Date()) {
  tidyquant::tq_get(series_id, from = start, to = end,
                    get = "economic.data")
}

#' Get Fama-French Data
#'
#' @param dataset 'Fama/French 5 Factors (2x3)' or 'Fama/French 3 Factors'
#'
#' @details
#' "Fama/French 3 Factors"
#' "Fama/French 3 Factors [Weekly]"
#' "Fama/French 3 Factors [Daily]"
#' "Fama/French 5 Factors (2x3)"
#' "Fama/French 5 Factors (2x3) [Daily]"
#' "Portfolios Formed on Size"
#' "Portfolios Formed on Size [ex.Dividends]"
#' "Portfolios Formed on Size [Daily]"
#' "Portfolios Formed on Book-to-Market"
#' "Portfolios Formed on Book-to-Market [ex. Dividends]"
#' "Portfolios Formed on Book-to-Market [Daily]"
#' "Portfolios Formed on Operating Profitability"
#' "Portfolios Formed on Operating Profitability [ex. Dividends]"
#' "Portfolios Formed on Operating Profitability [Daily]"
#' "Portfolios Formed on Investment"
#' "Portfolios Formed on Investment [ex. Dividends]"
#' "Portfolios Formed on Investment [Daily]"
#' "6 Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [ex. Dividends]"
#' "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Weekly]"
#' "6 Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "25 Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [ex. Dividends]"
#' "25 Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "100 Portfolios Formed on Size and Book-to-Market (10 x 10)"
#' "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [ex. Dividends]"
#' "100 Portfolios Formed on Size and Book-to-Market (10 x 10) [Daily]"
#' "6 Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [ex. Dividends]"
#' "6 Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "25 Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 Portfolios Formed on Size and Operating Profitability (5 x 5) [ex. Dividends]"
#' "25 Portfolios Formed on Size and Operating Profitability	(5 x 5) [Daily]"
#' "100 Portfolios Formed on Size and Operating Profitability (10 x 10)"
#' "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [ex. Dividends]"
#' "100 Portfolios Formed on Size and Operating Profitability (10 x 10) [Daily]"
#' "6 Portfolios Formed on Size and Investment (2 x 3)"
#' "6 Portfolios Formed on Size and Investment (2 x 3) [ex. Dividends]"
#' "6 Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "25 Portfolios Formed on Size and Investment (5 x 5)"
#' "25 Portfolios Formed on Size and Investment (5 x 5) [ex. Dividends]"
#' "25 Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "100 Portfolios Formed on Size and Investment (10 x 10)"
#' "100 Portfolios Formed on Size and Investment (10 x 10) [ex. Dividends]"
#' "100 Portfolios Formed on Size and Investment (10 x 10) [Daily]"
#' "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5)"
#' "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [ex. Dividends]"
#' "25 Portfolios Formed on Book-to-Market and Operating Profitability (5 x 5) [Daily]"
#' "25 Portfolios Formed on Book-to-Market and Investment (5 x 5)"
#' "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [ex. Dividends]"
#' "25 Portfolios Formed on Book-to-Market and Investment (5 x 5) [Daily]"
#' "25 Portfolios Formed on Operating Profitability and Investment (5 x 5)"
#' "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [ex. Dividends]"
#' "25 Portfolios Formed on Operating Profitability and Investment (5 x 5) [Daily]"
#' "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4) [ex. Dividends]"
#' "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4) [ex. Dividends]"
#' "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "32 Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4) [ex. Dividends]"
#' "Portfolios Formed on Earnings/Price"
#' "Portfolios Formed on Earnings/Price [ex. Dividends]"
#' "Portfolios Formed on Cashflow/Price"
#' "Portfolios Formed on Cashflow/Price [ex. Dividends]"
#' "Portfolios Formed on Dividend Yield"
#' "Portfolios Formed on Dividend Yield [ex. Dividends]"
#' "6 Portfolios Formed on Size and Earnings/Price"
#' "6 Portfolios Formed on Size and Earnings/Price [ex. Dividends]"
#' "6 Portfolios Formed on Size and Cashflow/Price"
#' "6 Portfolios Formed on Size and Cashflow/Price [ex. Dividends]"
#' "6 Portfolios Formed on Size and Dividend Yield"
#' "6 Portfolios Formed on Size and Dividend Yield [ex. Dividends]"
#' "Momentum Factor (Mom)"
#' "Momentum Factor (Mom) [Daily]"
#' "6 Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "25 Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "10 Portfolios Formed on Momentum"
#' "10 Portfolios Formed on Momentum [Daily]"
#' "Short-Term Reversal Factor (ST Rev)"
#' "Short-Term Reversal Factor (ST Rev) [Daily]"
#' "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3)"
#' "6 Portfolios Formed on Size and Short-Term Reversal (2 x 3) [Daily]"
#' "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5)"
#' "25 Portfolios Formed on Size and Short-Term Reversal (5 x 5) [Daily]"
#' "10 Portfolios Formed on Short-Term Reversal"
#' "10 Portfolios Formed on Short-Term Reversal [Daily]"
#' "Long-Term Reversal Factor (LT Rev)"
#' "Long-Term Reversal Factor (LT Rev) [Daily]"
#' "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3)"
#' "6 Portfolios Formed on Size and Long-Term Reversal (2 x 3) [Daily]"
#' "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5)"
#' "25 Portfolios Formed on Size and Long-Term Reversal (5 x 5) [Daily]"
#' "10 Portfolios Formed on Long-Term Reversal"
#' "10 Portfolios Formed on Long-Term Reversal [Daily]"
#' "Portfolios Formed on Accruals"
#' "25 Portfolios Formed on Size and Accruals"
#' "Portfolios Formed on Market Beta"
#' "25 Portfolios Formed on Size and Market Beta"
#' "Portfolios Formed on Net Share Issues"
#' "25 Portfolios Formed on Size and Net Share Issues"
#' "Portfolios Formed on Variance"
#' "25 Portfolios Formed on Size and Variance"
#' "Portfolios Formed on Residual Variance"
#' "25 Portfolios Formed on Size and Residual Variance"
#' "5 Industry Portfolios"
#' "5 Industry Portfolios [ex. Dividends]"
#' "5 Industry Portfolios [Daily]"
#' "10 Industry Portfolios"
#' "10 Industry Portfolios [ex. Dividends]"
#' "10 Industry Portfolios [Daily]"
#' "12 Industry Portfolios"
#' "12 Industry Portfolios [ex. Dividends]"
#' "12 Industry Portfolios [Daily]"
#' "17 Industry Portfolios"
#' "17 Industry Portfolios [ex. Dividends]"
#' "17 Industry Portfolios [Daily]"
#' "30 Industry Portfolios"
#' "30 Industry Portfolios [ex. Dividends]"
#' "30 Industry Portfolios [Daily]"
#' "38 Industry Portfolios"
#' "38 Industry Portfolios [ex. Dividends]"
#' "38 Industry Portfolios [Daily]"
#' "48 Industry Portfolios"
#' "48 Industry Portfolios [ex. Dividends]"
#' "48 Industry Portfolios [Daily]"
#' "49 Industry Portfolios"
#' "49 Industry Portfolios [ex. Dividends]"
#' "49 Industry Portfolios [Daily]"
#' "ME Breakpoints"
#' "BE/ME Breakpoints"
#' "Operating Profitability Breakpoints"
#' "Investment Breakpoints"
#' "E/P Breakpoints"
#' "CF/P Breakpoints"
#' "D/P Breakpoints"
#' "Prior (2-12) Return Breakpoints"
#' "Fama/French Developed 3 Factors"
#' "Fama/French Developed 3 Factors [Daily]"
#' "Fama/French Developed ex US 3 Factors"
#' "Fama/French Developed ex US 3 Factors [Daily]"
#' "Fama/French European 3 Factors"
#' "Fama/French European 3 Factors [Daily]"
#' "Fama/French Japanese 3 Factors"
#' "Fama/French Japanese 3 Factors [Daily]"
#' "Fama/French Asia Pacific ex Japan 3 Factors"
#' "Fama/French Asia Pacific ex Japan 3 Factors [Daily]"
#' "Fama/French North American 3 Factors"
#' "Fama/French North American 3 Factors [Daily]"
#' "Fama/French Developed 5 Factors"
#' "Fama/French Developed 5 Factors [Daily]"
#' "Fama/French Developed ex US 5 Factors"
#' "Fama/French Developed ex US 5 Factors [Daily]"
#' "Fama/French European 5 Factors"
#' "Fama/French European 5 Factors [Daily]"
#' "Fama/French Japanese 5 Factors"
#' "Fama/French Japanese 5 Factors [Daily]"
#' "Fama/French Asia Pacific ex Japan 5 Factors"
#' "Fama/French Asia Pacific ex Japan 5 Factors [Daily]"
#' "Fama/French North American 5 Factors"
#' "Fama/French North American 5 Factors [Daily]"
#' "Developed Momentum Factor (Mom)"
#' "Developed Momentum Factor (Mom) [Daily]"
#' "Developed ex US Momentum Factor (Mom)"
#' "Developed ex US Momentum Factor (Mom) [Daily]"
#' "European Momentum Factor (Mom)"
#' "European Momentum Factor (Mom) [Daily]"
#' "Japanese Momentum Factor (Mom)"
#' "Japanese Momentum Factor (Mom) [Daily]"
#' "Asia Pacific ex Japan Momentum Factor (Mom)"
#' "Asia Pacific ex Japan Momentum Factor (Mom) [Daily]"
#' "North American Momentum Factor (Mom)"
#' "North American Momentum Factor (Mom) [Daily]"
#' "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 Developed Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 Developed ex US Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "6 European Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 European Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 Japanese Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 North American Portfolios Formed on Size and Book-to-Market (2 x 3) [Daily]"
#' "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 Developed Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 Developed ex US Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "25 European Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 European Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 Japanese Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5)"
#' "25 North American Portfolios Formed on Size and Book-to-Market (5 x 5) [Daily]"
#' "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 Developed Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 Developed ex US Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "6 European Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 European Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 Japanese Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 North American Portfolios Formed on Size and Operating Profitability (2 x 3) [Daily]"
#' "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 Developed Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]"
#' "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 Developed ex US Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]"
#' "25 European Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 European Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]"
#' "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 Japanese Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]"
#' "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5)"
#' "25 North American Portfolios Formed on Size and Operating Profitability (5 x 5) [Daily]"
#' "6 Developed Portfolios Formed on Size and Investment (2 x 3)"
#' "6 Developed Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3)"
#' "6 Developed ex US Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "6 European Portfolios Formed on Size and Investment (2 x 3)"
#' "6 European Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "6 Japanese Portfolios Formed on Size and Investment (2 x 3)"
#' "6 Japanese Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3)"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "6 North American Portfolios Formed on Size and Investment (2 x 3)"
#' "6 North American Portfolios Formed on Size and Investment (2 x 3) [Daily]"
#' "25 Developed Portfolios Formed on Size and Investment (5 x 5)"
#' "25 Developed Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5)"
#' "25 Developed ex US Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "25 European Portfolios Formed on Size and Investment (5 x 5)"
#' "25 European Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "25 Japanese Portfolios Formed on Size and Investment (5 x 5)"
#' "25 Japanese Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5)"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "25 North American Portfolios Formed on Size and Investment (5 x 5)"
#' "25 North American Portfolios Formed on Size and Investment (5 x 5) [Daily]"
#' "6 Developed Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 Developed Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 Developed ex US Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "6 European Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 European Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "6 Japanese Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 Japanese Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "6 North American Portfolios Formed on Size and Momentum (2 x 3)"
#' "6 North American Portfolios Formed on Size and Momentum (2 x 3) [Daily]"
#' "25 Developed Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 Developed Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 Developed ex US Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "25 European Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 European Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "25 Japanese Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 Japanese Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 Asia Pacific ex Japan Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "25 North American Portfolios Formed on Size and Momentum (5 x 5)"
#' "25 North American Portfolios Formed on Size and Momentum (5 x 5) [Daily]"
#' "32 Developed Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 European Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 Japanese Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 North American Portfolios Formed on Size, Book-to-Market, and Operating Profitability (2 x 4 x 4)"
#' "32 Developed Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 Developed ex US Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 European Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 Japanese Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 Asia Pacific ex Japan Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 North American Portfolios Formed on Size, Book-to-Market, and Investment (2 x 4 x 4)"
#' "32 Developed Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "32 Developed ex US Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "32 European Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "32 Japanese Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "32 Asia Pacific ex Japan Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "32 North American Portfolios Formed on Size, Operating Profitability, and Investment (2 x 4 x 4)"
#' "Fama/French Emerging 5 Factors"
#' "Emerging Momentum Factor (Mom)"
#' "6 Emerging Market Portfolios Formed on Size and Book-to-Market (2 x 3)"
#' "6 Emerging Market Portfolios Formed on Size and Operating Profitability (2 x 3)"
#' "6 Emerging Market Portfolios Formed on Size and Investment (2 x 3)"
#' "6 Emerging Market Portfolios Formed on Size and Momentum (2 x 3)"
#' "4 Emerging Market Portfolios Formed on Book-to-Market  and Operating Profitability (2 x 2)  "
#' "4 Emerging Market Portfolios Formed on Operating Profitability and Investment (2 x 2)"
#' "4 Emerging Market Portfolios Formed on Book-to-Market and Investment (2 x 2)"
#'
#' @return
#' @export
#'
#' @examples
get_famafrench <- function(dataset = "Fama/French 5 Factors (2x3)") {
  out <- frenchdata::download_french_data(dataset)

  out <- as.list(out$subsets)$data[[1]]

  out |>
    dplyr::mutate(date = case_when(stringr::str_length(date) == 6 ~ ym(date),
                            stringr::str_length(date) == 8 ~ ymd(date),
                            T ~ lubridate::NA_Date_)) |>
    dplyr::mutate_if(is.numeric, function(x) x / 100)
}
