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
#' # Run trackR classic inside RStudio
#' \dontrun{trackR(launch.browser = rstudioapi::viewer)}
#'
#' @export
trackR <- function(...) {
  shiny::runApp(paste0(find.package("trackR"), "/trackR"), ...)
}

#' @title Fix Tracking Errors
#'
#' @description Simple Shiny-based GUI to fix a posteriori common tracking errors,
#'  such as removing unwanted tracks, fixing swapped track IDs and reconnecting
#'  tracks.
#'
#' @param ... Arguments passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything. A file is saved if the
#'  save button is used in the app.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # Run trackFixer in an external window
#' \dontrun{trackFixer()}
#'
#' @export
trackFixer <- function(...) {
  shiny::runApp(paste0(find.package("trackR"), "/trackFixer"), ...)
}


#' @title Display and Export Video with Track Overlay
#'
#' @description Simple Shiny-based GUI to display and export a video with tracks
#'  overlaid on top.
#'
#' @param ... Arguments passed to \link[shiny]{runApp}.
#'
#' @return This function does not return anything. A file is saved if the
#'  export button is used in the app.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
#' @export
trackPlayer <- function(...) {
  shiny::runApp(paste0(find.package("trackR"), "/trackPlayer"), ...)
}

#' @export
videoFixer <- function(...) {
  shiny::runApp(paste0(find.package("trackR"), "/videoFixer"), ...)
}
