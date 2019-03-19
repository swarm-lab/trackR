#' @export
moduleUI <- function(module) {
  base <- system.file("modules/", package = "trackR")

  if (!file.exists(paste0(base, module, "UI.R")))
    stop("Unknown module.")

  source(paste0(base, module, "UI.R"), local = TRUE)$value
}


#' @export
moduleServer <- function(module) {
  base <- system.file("modules/", package = "trackR")

  if (!file.exists(paste0(base, module, "Server.R")))
    stop("Unknown module.")

  source(paste0(base, module, "Server.R"), local = parent.frame(n = 1))
}
