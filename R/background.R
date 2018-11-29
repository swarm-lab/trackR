backgrounder <- function(video, n = 10, method = "mean") {
  if (!Rvision::isVideo(video))
    stop("This is not a Video object.")

  if (n > Rvision::nframes(video))
    stop("n should be smaller than the total number of frames in the video.")

  shiny::showNotification("Loading images in memory.", id = "load", duration = NULL)

  frames <- round(seq.int(1, Rvision::nframes(video) - 2, length.out = n))

  l1 <- c()
  for (i in 1:n) {
    l1 <- c(l1, Rvision::readFrame(video, frames[i]))
  }

  shiny::removeNotification(id = "load")
  shiny::showNotification("Calculating background.", id = "calc", duration = NULL)

  if (method == "mean") {
    out <- Rvision::mean(l1)
  } else if (method == "median") {
    l2 <- lapply(l1, as.array)

    l3 <- list()
    for (i in 1:Rvision::nchan(l1[[1]])) {
      l3[[i]] <- simplify2array(lapply(l2, function(m, i) m[, , i], i = i))
    }

    mat <- array(NA, dim = dim(l1[[1]]))
    for (i in 1:Rvision::nchan(l1[[1]])) {
      mat[, , i] <- trackR:::applyShiny(l3[[i]], c(1, 2), stats::median.default,
                               message = paste0("Processing channel ", i,
                                                " out of ", Rvision::nchan(l1[[1]])))
    }

    if (Rvision::bitdepth(l1[[1]]) == "8U") {
      mat <- mat * 256
      out <- Rvision::changeBitDepth(Rvision::image(mat), 8)
    } else {
      out <- Rvision::image(mat)
    }
  } else {
    stop("'method' should be 'mean' or 'median'")
  }

  shiny::removeNotification(id = "calc")

  out
}




