#' @title Simple Video Tracking Software
#'
#' @description Simple video tracking software with Shiny-based GUI.
#'
#' @param tracker A character string indicating the tracking algorithm to use
#'  (see 'Details').
#'
#' @param ... Arguments passed to \link[shiny]{runApp}.
#'
#' @details \code{trackR} provides three different tracking algorithms:
#'  \describe{
#'     \item{\code{"classic"}:}{This algorithm uses basic background subtraction
#'       to segment the objects in the image. A background image can be provided
#'       by the user or can be computed as the mean or median of \code{n} images
#'       taken at regular intervals throughout the video. \code{"classic"} should
#'       work well in most lab situations with a constant and homogenous background.}
#'     \item{\code{"quanta"}:}{This algorithm uses quantized background subtraction
#'       to segment the objects in the image. In a quantized background image,
#'       each pixel corresponds to the lower (if the objects are generally darker
#'       than the background) or upper (if the objects are generally lighter than
#'       the background) 2.5 percentile of \code{n} images taken at regular
#'       intervals throughout the video.  A pixel outside of these bounds in a
#'       given image would be considered as being part of an object.
#'       \code{"quanta"} should work better than \code{"classic"} in situations
#'       were the background is constant but not homogenous (e.g. if the
#'       background has different shades).}
#'     \item{\code{"tracktor"}:}{This algorithm uses adaptive thresholding to
#'       segment the objects in the image. It is a liberal implementation of the
#'       \href{https://github.com/vivekhsridhar/tracktor}{tracktor} algorithm
#'       developed for Python by Vivek Hari Sridhar, Dominique G. Roche and Simon
#'       Gingins. \code{"tracktor"} should work better than \code{"classic"} and
#'       \code{"quanta"} in situations were the background is not constant (e.g.
#'       when the environment changes over time). See \emph{"Sridhar, V. H., Roche,
#'       D. G., and Gingins, S. (2019). Tracktor: Image-based automated tracking
#'       of animal movement and behaviour. Methods Ecol. Evol. 10, 691.
#'       doi:10.1111/2041-210X.13166"} for more details.}
#'  }
#'
#' @return This function does not return anything. A file is saved if the
#'  tracking is successfully completed.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # Run trackR classic in an external window
#' \dontrun{trackR("classic")}
#'
#' # Run trackR quanta in an external window
#' \dontrun{trackR("quanta")}
#'
#' # Run trackR tracktor in an external window
#' \dontrun{trackR("tracktor")}
#'
#' # Run trackR classic inside RStudio
#' \dontrun{trackR("classic", launch.browser = rstudioapi::viewer)}
#'
#' # Run trackR quanta inside RStudio
#' \dontrun{trackR("quanta", launch.browser = rstudioapi::viewer)}
#'
#' # Run trackR tracktor inside RStudio
#' \dontrun{trackR("tracktor", launch.browser = rstudioapi::viewer)}
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


#' @export
trackPlayer <- function(video, tracks, delay = 10, save = FALSE) {
  vid <- Rvision::video(video)
  df <- readr::read_csv(tracks)
  cbPalette <- c("#FFBF80", "#FF8000", "#FFFF99", "#FFFF33", "#B2FF8C", "#33FF00",
                 "#A6EDFF", "#1AB2FF", "#CCBFFF", "#664CFF", "#FF99BF", "#E61A33")

  if (save)
    vw <- Rvision::videoWriter(paste0(sub(basename(video), "", video), "tracks.", tools::file_ext(video)),
                               codec(vid), fps(vid), nrow(vid), ncol(vid))

  newDisplay("trackPlayer", nrow(vid), ncol(vid))

  for (i in 1:nframes(vid)) {
    idx <- df$frame > (i - Rvision::fps(vid)) & df$frame <= i
    sub_df <- df[idx, ]
    frame <- readNext(vid)

    for (j in unique(sub_df$track)) {
      idx <- sub_df$track == j

      if (length(sub_df[idx, ]$x) > 1) {
        Rvision::drawLine(frame, sub_df[idx, ]$x[1:(length(sub_df[idx, ]$x) - 1)],
                          sub_df[idx, ]$y[1:(length(sub_df[idx, ]$x) - 1)],
                          sub_df[idx, ]$x[2:length(sub_df[idx, ]$x)],
                          sub_df[idx, ]$y[2:length(sub_df[idx, ]$x)],
                          cbPalette[(sub_df[idx, ]$track %% 12) + 1],
                          round(ncol(frame) / 200))
      } else {
        Rvision::drawCircle(frame, sub_df[idx, ]$x, sub_df[idx, ]$y, 2, sub_df[idx, ]$track, 2)
      }
    }

    Rvision::display(frame, "trackPlayer", 1, nrow(vid), ncol(vid))

    if (save)
      writeFrame(vw, frame)
  }

  destroyDisplay("trackPlayer")

  if (save)
    release(vw)
}
