#' @title Reconstruct the Background of a Video
#'
#' @description This function attempts to reconstruct the background of a video
#'  that was filmed from a fixed point of view.
#'
#' @param video A video object as produced by \code{\link[Rvision]{video}}.
#'
#' @param n The number of images of the video that will be used to reconstruct
#'  the background.
#'
#' @param method The name of a method to reconstruct the background. There are
#'  currently 4 methods available: "median" (uses the median value of each pixel
#'  as background), "min" (uses the minimum value of each pixel as background),
#'  "max" (uses the maximum value of each pixel as background), and "quant"
#'  (uses an arbitrary quantile value of each pixel as background).
#'
#' @param prob If \code{method = "quant"}, the quantile value to use.
#'
#' @param start,end The start and end frames of the video to use for the
#'  background reconstruction. If not set, the first and last frames will be
#'  used.
#'
#' @return And \code{\link[Rvision]{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' cctv <- Rvision::video(system.file("sample/people.mp4", package = "trackR"))
#' background <- backgrounder(cctv, 20)
#' plot(background)
#'
#' @export
backgrounder <- function(video, n = 10, method = "median", prob = 0.025,
                         start = NULL, end = NULL) {
  if (!Rvision::isVideo(video) & !Rvision::isVideoStack(video))
    stop("This is not a Video or VideoStack object.")

  if (n > Rvision::nframes(video))
    stop("n should be smaller than the total number of frames in the video.")

  if (is.null(start))
    start <- 1

  if (is.null(end))
    end <- Rvision::nframes(video) - 1

  if (shiny::isRunning()) {
    shiny::showNotification("Loading images in memory.", id = "load", duration = NULL)
  } else {
    message("Loading images in memory.")
  }

  frames <- round(seq.int(start, end, length.out = n))

  l <- lapply(frames, function(i) {
    Rvision::readFrame(video, i)
  })

  if (shiny::isRunning()) {
    shiny::removeNotification(id = "load")
    shiny::showNotification("Calculating background.", id = "calc", duration = NULL)
  } else {
    message("Calculating background.")
  }

  if (method == "mean") {
    out <- mean(l)
  } else {
    med <- array(0.0, dim = c(nrow(video), ncol(video), 3))

    for (i in 1:3) {
      if (shiny::isRunning()) {
        shiny::showNotification(paste0("Processing ", c("red", "green", "blue")[i], " channel."),
                                id = "layer", duration = NULL)
      } else {
        message(paste0("Processing ", c("red", "green", "blue")[i], " channel."))
      }

      if (method == "median") {
        med[, , i] <- Rfast::rowMedians(sapply(l, function(x) x[][, , i]))
      } else if (method == "min") {
        med[, , i] <- Rfast::rowMins(sapply(l, function(x) x[][, , i]), value = TRUE)
      } else if (method == "max") {
        med[, , i] <- Rfast::rowMaxs(sapply(l, function(x) x[][, , i]), value = TRUE)
      } else if (method == "quant") {
        med[, , i] <- Rfast2::rowQuantile(sapply(l, function(x) x[][, , i]), probs = prob)
      } else {
        stop("'method' should be one of 'mean', 'median', 'min', 'max', or 'quant'")
      }

      if (shiny::isRunning()) {
        shiny::removeNotification(id = "layer")
      }
    }
    mode(med) <- "integer"
    out <- Rvision::image(med)
  }

  if (shiny::isRunning()) {
    shiny::removeNotification(id = "calc")
  }

  out
}
