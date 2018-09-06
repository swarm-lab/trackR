#' @export
trackR <- function() {
  app_path <- paste0(find.package("trackR"), "/app")
  shiny::runApp(app_path)
}
