#' @title Return Scatter Plot
#'
#' @param series1
#' @param series2
#' @param data
#' @param field
#' @param name1
#' @param name2
#' @param title
#' @param trend
#' @param timescale one of "alpha", "color", or "none"
#' @param labels one of "dates" or "none"
#' @param pts.size
#' @param pts.color
#' @param lm.size
#' @param lm.color
#'
#' @return
#' @export
#'
#' @examples
chart_returnscatter <- function(series1, series2,
                                data = "prices", field = "adjusted",
                                name1 = NULL, name2 = NULL,
                                title = "Return Scatter Plot",
                                subtitle = "dates",
                                trend = T,
                                timescale = "alpha",
                                labels = "dates",
                                pts.size = 0.1, pts.color = "black",
                                lm.size = 0.1, lm.color = "red") {
  if (data == "prices") {
    r1 <- series1 |>
      dplyr::mutate(r = !!as.name(field)/dplyr::lag(!!as.name(field))-1)
    r2 <- series2 |>
      dplyr::mutate(r = !!as.name(field)/dplyr::lag(!!as.name(field))-1)
  } else if (data == "returns") {
    r1 <- series1 |>
      dplyr::mutate(r = !!as.name(field))
    r2 <- series2 |>
      dplyr::mutate(r = !!as.name(field))
  } else {
    stop("data must be either 'prices' or 'returns'")
  }

  df <- dplyr::full_join(r1, r2, by = "date") |>
    dplyr::filter(!is.na(r.x), !is.na(r.y))

  if (is.null(name1)) { name1 <- series1[[1,"symbol"]] }
  if (is.null(name2)) { name2 <- series2[[1,"symbol"]] }
  if (subtitle == "dates") { subtitle <- paste0(min(df$date), " to ", max(df$date)) }

  df |>
    ggplot2::ggplot(ggplot2::aes(x = r.x, y = r.y)) +
    {if (timescale == "alpha") { ggplot2::geom_point(size = pts.size, color = pts.color,
                                                     ggplot2::aes(alpha = date)) }} +
    {if (timescale == "color") { ggplot2::geom_point(size = pts.size,
                                                     ggplot2::aes(color = date)) }} +
    {if (!(timescale %in% c("alpha", "color"))) { ggplot2::geom_point(size = pts.size, color = pts.color) }} +
    {if (trend) { ggplot2::geom_smooth(method = "lm", se = FALSE, size = lm.size, color = lm.color) }} +
    {if (label == "dates") { ggrepel::geom_text_repel(color = pts.color, ggplot2::aes(label = format(date)), size = 2.5, max.time = 2) }} +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = name1, y = name2, color = NULL, alpha = NULL) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_color_viridis_c(option = "plasma")
}

#' @title Cumulative Growth Chart
#'
#' @param ...
#' @param data
#' @param field
#' @param fieldname
#' @param base
#' @param name
#' @param title
#' @param subtitle
#' @param timescale
#' @param labels
#' @param pts.size
#' @param pts.color
#'
#' @return
#' @export
#'
#' @examples
chart_cumgrowth <- function(..., data = "prices", field = "adjusted",
                            fieldname = "symbol",
                            base = 100, yscalelog = F,
                            name = NULL, title = "Cumulative Growth Chart",
                            subtitle = "dates", timescale = "alpha",
                            labels = "dates", pts.size = 0.1, pts.color = "black") {
  data_list <- list(...)
  # min date for each
  min_date <- max(sapply(data_list, function(x) min(x$date)))
  if (data == "prices") {
    df <- dplyr::bind_rows(data_list) |>
      dplyr::group_by(!!as.name(fieldname)) |>
      dplyr::mutate(r = !!as.name(field)/dplyr::lag(!!as.name(field))-1) |>
      dplyr::filter(!is.na(r),
                    date >= min_date) |>
      dplyr::mutate(p = cumprod(1+r))
  } else if (data == "returns") {
    df <- dplyr::bind_rows(data_list) |>
      dplyr::group_by(!!as.name(fieldname)) |>
      dplyr::mutate(r = !!as.name(field)) |>
      dplyr::filter(!is.na(r),
                    date >= min_date) |>
      dplyr::mutate(p = cumprod(1+r))
  } else {
    stop("data must be either 'prices' or 'returns'")
  }

  if (subtitle == "dates") { subtitle <- paste0(min(df$date), " to ", max(df$date)) }

  df |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = p*base, color = !!as.name(fieldname))) +
    ggplot2::geom_line() +
    {if (yscalelog) { ggplot2::scale_y_log10() }}
}


