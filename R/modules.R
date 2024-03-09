#' @title Source Module UI
#'
#' @description Utility function to load the different UI modules of the apps.
#'
#' @param module The module name in the form "app/module".
#'
#' @return The UI code of the module.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' moduleUI("trackR/video")
#' }
#'
#' @export
moduleUI <- function(module) {
  base <- system.file("modules", package = "trackR")

  if (!file.exists(paste0(base, "/", module, "UI.R"))) {
    stop("Unknown module.")
  }

  source(paste0(base, "/", module, "UI.R"), local = TRUE)$value
}


#' @title Source Module Server
#'
#' @description Utility function to load the different server modules of the apps.
#'
#' @param module The module name in the form "app/module".
#'
#' @return The server code of the module.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' moduleSVR("trackR/video")
#' }
#'
#' @export
moduleSVR <- function(module) {
  base <- system.file("modules", package = "trackR")

  if (!file.exists(paste0(base, "/", module, "Server.R"))) {
    stop("Unknown module.")
  }

  source(paste0(base, "/", module, "Server.R"), local = parent.frame(n = 1))
}


#' @export
load_module <- function(app_folder, module, type = "UI") {
  to_load <- paste0(app_folder, "/src/", module, type, ".R")

  if (!file.exists(to_load)) {
    stop("Unknown module.")
  }

  if (type == "UI") {
    source(to_load, local = TRUE)$value
  } else if (type == "Server") {
    source(to_load, local = parent.frame(n = 1))
  } else {
    stop("Invalid type.")
  }
}
