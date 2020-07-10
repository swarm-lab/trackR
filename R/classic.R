#' @export
backgrounderClassic <- function(video, n = 10, method = "median", start = NULL, end = NULL) {
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

  l1 <- c()
  for (i in 1:n) {
    l1 <- c(l1, Rvision::readFrame(video, frames[i]))
  }

  shiny::removeNotification(id = "load")
  shiny::showNotification("Calculating background.", id = "calc", duration = NULL)

  if (method == "mean") {
    out <- Rvision::mean(l1)
  } else if (method == "median") {
    med <- array(0.0, dim = c(nrow(video), ncol(video), 3))

    for (i in 1:3) {
      shiny::showNotification(paste0("Processing ", c("red", "green", "blue")[i], " channel."),
                              id = "layer", duration = NULL)
      med[, , i] <- matrixStats::rowMedians(sapply(l1, function(x) x[][, , i]))
      shiny::removeNotification(id = "layer")
    }
    mode(med) <- "integer"
    out <- Rvision::image(med)
  } else {
    stop("'method' should be 'mean' or 'median'")
  }

  shiny::removeNotification(id = "calc")

  out
}


#' @export
blobClassic <- function(x, background, mask = NULL, smoothing, threshold) {
  dif <- Rvision::gaussianBlur(Rvision::absdiff(x, background), 11, 11, smoothing, smoothing)

  if (Rvision::isImage(mask)) {
    dif <- dif * (mask / 255)
  }

  bin <- Rvision::changeColorSpace(dif, "GRAY") > threshold
  Rvision::connectedComponents(bin, 8)
}


#' @export
pipelineClassic <- function(video, begin, end, background, mask = NULL, smoothing,
                            threshold, minSize, maxSize, lookBack, maxDist, blobsizes,
                            progress = FALSE, display = FALSE, quality = 1, scale = 1) {

  n <- (end - begin + 1)
  tracks <- data.frame(id = numeric(n), x = numeric(n), y = numeric(n),
                       size = numeric(n), frame = numeric(n) - 2 * lookBack,
                       track = numeric(n))
  pos <- 0
  bs <- stats::median(blobsizes)
  bs_thresh <- bs + 1.5 * stats::IQR(blobsizes)

  cbPalette <- c("#FFBF80", "#FF8000", "#FFFF99", "#FFFF33", "#B2FF8C", "#33FF00",
                 "#A6EDFF", "#1AB2FF", "#CCBFFF", "#664CFF", "#FF99BF", "#E61A33")

  if (progress) {
    pb <- shiny::Progress$new()
    on.exit(pb$close())
    pb$set(message = "Computing tracks", value = 0, detail = "0%")
    nFrames <- length(begin:end)
    old <- 0
    oldFrame <- begin
    oldTime <- Sys.time()
  }

  local_bg <- NA
  local_mask <- NA

  if (quality < 1) {
    local_bg <- Rvision::resize(background, fx = quality, fy = quality, interpolation = "area")

    if (Rvision::isImage(mask)) {
      local_mask <- Rvision::resize(mask, fx = quality, fy = quality, interpolation = "area")
    }
  } else {
    local_bg <- Rvision::cloneImage(background)

    if (Rvision::isImage(mask)) {
      local_mask <- Rvision::cloneImage(mask)
    }
  }

  Rvision::setProp(video, "POS_FRAMES", begin - 1)

  for (i in begin:end) {
    idx <- tracks$frame > (i - lookBack) & tracks$frame < i
    past <- tracks[idx, ]

    if (quality < 1) {
      frame <- Rvision::resize(Rvision::readNext(video), fx = quality, fy = quality,
                               interpolation = "area")
    } else {
      frame <- Rvision::readNext(video)
    }

    if (Rvision::isImage(local_mask)) {
      cc <- blobClassic(frame, local_bg, local_mask, smoothing, threshold)
    } else {
      cc <- blobClassic(frame, local_bg, NULL, smoothing, threshold)
    }

    if (nrow(cc$table) > 0) {
      blobs <- do.call(rbind, by(cc$table, cc$table[, "id"],
                                 function(x) data.frame(id = x[1, "id"],
                                                        x = mean(x[, "x"]),
                                                        y = mean(x[, "y"]),
                                                        size = nrow(x))))
      blobs <- blobs[blobs$size >= minSize & blobs$size <= maxSize, ]

      if (nrow(blobs) > 0) {
        fat <- blobs$size > bs_thresh
        if (any(fat)) {
          idx <- blobs$id[fat]

          for (j in seq_len(length(idx))) {
            k <- round(blobs$size[blobs$id == idx[j]] / bs)
            xy <- cc$table[cc$table$id == idx[j], c("x", "y")]
            clust <- stats::kmeans(xy, k)$cluster

            blobs <- rbind(blobs[-which(blobs$id == idx[j]), ],
                           do.call(rbind, by(cbind(xy, clust), clust,
                                             function(x) data.frame(id = x[1, "clust"] + max(blobs$id),
                                                                    x = mean(x[, "x"]),
                                                                    y = mean(x[, "y"]),
                                                                    size = nrow(x)))))
          }
        }

        blobs$frame <- i
        blobs$track <- NA
        tmp <- simplerTracker(blobs, past, maxDist = maxDist)

        newTrack <- is.na(tmp$track)
        if (sum(newTrack) > 0) {
          tmp$track[newTrack] <- seq(max(tracks$track) + 1, max(tracks$track) + sum(newTrack), 1)
        }

        nRows <- nrow(tmp)
        if ((pos + nRows) > n) {
          tracks[(n + 1):(2 * n + nRows), ] <- 0
          n <- nrow(tracks)
        }
        tracks[(pos + 1):(pos + nRows), ] <- tmp
        pos <- pos + nRows
      }
    }

    if (progress) {
      new <- floor(100 * (i - begin + 1) / nFrames)
      if (new > old) {
        newTime <- Sys.time()
        fps <- (i - oldFrame + 1) / as.numeric(difftime(newTime, oldTime, units = "secs"))
        old <- new
        oldFrame <- i
        oldTime <- newTime
        pb$set(value = old / 100, detail = paste0(old, "% - ", round(fps, digits = 2), "fps"))
      }
    }

    if (display) {
      idx <- tracks$track > 0 & tracks$frame > (i - Rvision::fps(video)) & tracks$frame <= i
      tmp <- tracks[idx, ]

      for (j in unique(tmp$track)) {
        idx <- tmp$track == j

        if (length(tmp[idx, ]$x) > 1) {
          Rvision::drawLine(frame, tmp[idx, ]$x[1:(length(tmp[idx, ]$x) - 1)],
                            tmp[idx, ]$y[1:(length(tmp[idx, ]$x) - 1)],
                            tmp[idx, ]$x[2:length(tmp[idx, ]$x)],
                            tmp[idx, ]$y[2:length(tmp[idx, ]$x)],
                            cbPalette[(tmp[idx, ]$track %% 12) + 1],
                            round(ncol(frame) / 200))
        } else {
          Rvision::drawCircle(frame, tmp[idx, ]$x, tmp[idx, ]$y, 2, tmp[idx, ]$track, 2)
        }
      }

      Rvision::display(frame, "trackR", 1, nrow(frame) * scale / quality, ncol(frame) * scale / quality)
    }
  }

  tracks[1:pos, ]
}
