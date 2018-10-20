#' @title Simple Video Tracking Software
#'
#' @description Simple video tracking software with Shiny-based GUI.
#'
#' @param ... Arguments passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything. A file is saved if the
#'  tracking is successfully completed.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # Run trackR in an external window
#' \dontrun{trackR()}
#'
#' # Run trackR inside RStudio
#' \dontrun{trackR(launch.browser = rstudioapi::viewer)}
#'
#' @export
trackR <- function(...) {
  app_path <- paste0(find.package("trackR"), "/trackR")
  shiny::runApp(app_path, ...)
}
