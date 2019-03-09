#' @title Simple Video Tracking Software
#'
#' @description Simple video tracking software with Shiny-based GUI.
#'
#' @param tracker A character string indicating the tracking algorithm to use
#'  (see 'Details').
#'
#' @param ... Arguments passed to \link[shiny]{runApp}.
#'
#' @details TODO
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
trackR <- function(tracker = "classic", ...) {
  app_path <- switch(
    tracker,
    "classic" = paste0(find.package("trackR"), "/trackR_classic"),
    "tracktor" = paste0(find.package("trackR"), "/trackR_tracktor"),
    "quanta" = paste0(find.package("trackR"), "/trackR_quanta"),
    stop("This tracker is not supported.")
  )

  shiny::runApp(app_path, ...)
}

# trackR <- function(...) {
#   app_path <- paste0(find.package("trackR"), "/trackR")
#   shiny::runApp(app_path, ...)
# }
