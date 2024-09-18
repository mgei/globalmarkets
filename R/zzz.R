.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(ggplot2::theme_minimal())
  invisible(Sys.setlocale("LC_TIME", "en_US.UTF-8"))
}
