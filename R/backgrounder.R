#' @export
backgrounder <- function(video, n = 10, method = "median", prob = 0.025,
                         start = NULL, end = NULL) {
  if (!Rvision::isVideo(video))
    stop("This is not a Video object.")

  if (n > Rvision::nframes(video))
    stop("n should be smaller than the total number of frames in the video.")

  if (is.null(start))
    start <- 1

  if (is.null(end))
    end <- nframes(video) - 1

  shiny::showNotification("Loading images in memory.", id = "load", duration = NULL)

  frames <- round(seq.int(start, end, length.out = n))

  l <- lapply(frames, function(i) {
    Rvision::readFrame(video, i)
  })

  shiny::removeNotification(id = "load")
  shiny::showNotification("Calculating background.", id = "calc", duration = NULL)

  if (method == "mean") {
    out <- mean(l)
  } else {
    med <- array(0.0, dim = c(nrow(video), ncol(video), 3))

    for (i in 1:3) {
      shiny::showNotification(paste0("Processing ", c("red", "green", "blue")[i], " channel."),
                              id = "layer", duration = NULL)
      if (method == "median") {
        med[, , i] <- Rfast::rowMedians(sapply(l, function(x) x[][, , i]))
      } else if (method == "min") {
        med[, , i] <- Rfast::rowMins(sapply(l, function(x) x[][, , i]), value = TRUE)
      } else if (method == "max") {
        med[, , i] <- Rfast::rowMaxs(sapply(l, function(x) x[][, , i]), value = TRUE)
      } else if (method == "quant") {
        med[, , i] <- Rfast2::rowQuantile(sapply(l, function(x) x[][, , i]), probs = prob)
      } else {
        stop("'method' should be one of 'mean', 'median', 'min', 'max', or 'quantile'")
      }

      shiny::removeNotification(id = "layer")
    }
    mode(med) <- "integer"
    out <- Rvision::image(med)
  }

  shiny::removeNotification(id = "calc")

  out
}
