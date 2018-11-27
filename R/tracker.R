pdiff <- function(a, b) {
  nr <- length(a)
  nc <- length(b)

  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)

  ma - mb
}

simpleTracker <- function(current, past, lookBack = 30, maxDist = 10) {
  if (nrow(past) == 0) {
    current$track <- 1:nrow(current)
    return(current)
  }

  i <- current$frame[1]
  trackCounter <- max(past$track)

  mat <- abs(trackR:::pdiff(current$x, past$x)) + abs(trackR:::pdiff(current$y, past$y))
  mat2 <- abs(trackR:::pdiff(current$frame, past$frame))
  maxMat <- matrix(maxDist * (i - past$frame), nrow = nrow(current),
                   ncol = nrow(past), byrow = TRUE)
  validMat <- mat <= maxMat
  mat <- mat * log(mat2 + 1)

  if (diff(dim(mat)) < 0) {
    stop(paste0("Error in image ", i))
  }

  h <- as.vector(clue::solve_LSAP(mat))
  tracks <- past$track[h]

  valid <- validMat[(h - 1) * nrow(mat) + 1:nrow(mat)]
  if (any(!valid)) {
    nNew <- sum(!valid)
    tracks[!valid] <- trackCounter + 1:nNew
    trackCounter <- trackCounter + nNew
  }

  dup <- duplicated(tracks) | duplicated(tracks, fromLast = TRUE)
  safe <- !dup
  h[safe] <- 0
  while (any(dup)) {
    safe[which.max(h)] <- TRUE
    h[safe] <- 0

    newCurrent <- current[!safe, ]
    newPast <- dplyr::filter(past, !(track %in% tracks[safe])) # REMOVE DPLYR DEPENDENCE HERE

    if (nrow(newCurrent) > nrow(newPast)) {
      n <- nrow(newCurrent) - nrow(newPast)
      newPast <- rbind(newPast, data.frame(id = NA, x = rep(-9999, n),
                                           y = rep(-9999, n), size = NA,
                                           frame = newCurrent$frame[1],
                                           track = NA))
    }

    mat <- abs(trackR:::pdiff(newCurrent$x, newPast$x)) + abs(trackR:::pdiff(newCurrent$y, newPast$y))
    mat2 <- abs(trackR:::pdiff(newCurrent$frame, newPast$frame))
    maxMat <- matrix(maxDist * (i - newPast$frame), nrow = nrow(newCurrent),
                     ncol = nrow(newPast), byrow = TRUE)
    validMat <- mat <= maxMat
    mat <- mat * mat2

    newH <- as.vector(clue::solve_LSAP(mat))
    newTracks <- newPast$track[newH]

    valid <- validMat[(newH - 1) * nrow(mat) + 1:nrow(mat)]
    if (any(!valid)) {
      nNew <- sum(!valid)
      newTracks[!valid] <- trackCounter + 1:nNew
      trackCounter <- trackCounter + nNew
    }

    tracks[!safe] <- newTracks

    dup <- duplicated(tracks) | duplicated(tracks, fromLast = TRUE)
    safe <- !dup
    h[safe] <- 0
  }

  current$track <- tracks
  current
}

simplerTracker <- function(current, past, maxDist = 10) {
  if (nrow(past) == 0) {
    current$track <- 1:nrow(current)
    return(current)
  }

  frames <- seq(max(past$frame), min(past$frame), -1)

  for (f in frames) {
    if (nrow(past) > 0 & sum(is.na(current$track)) > 0) {
      tmp <- past[past$frame == f, ]

      if (nrow(tmp) > 0) {
        mat <- abs(trackR:::pdiff(current$x, tmp$x)) + abs(trackR:::pdiff(current$y, tmp$y))
        mat[!is.na(current$track), ] <- max(mat) * 2

        if (nrow(mat) > ncol(mat)) {
          h <- rep(NA, nrow(mat))
          h[as.vector(clue::solve_LSAP(t(mat)))] <- seq(1, ncol(mat))
        } else {
          h <- as.vector(clue::solve_LSAP(mat))
        }
        valid <- mat[(h - 1) * nrow(mat) + 1:nrow(mat)] <= (maxDist * (current$frame[1] - f))
        h[!valid] <- NA
        current$track[!is.na(h)] <- tmp$track[h[!is.na(h)]]

        past <- past[past$frame != f, ]
        past <- past[!(past$track %in% tmp$track[h]), ]
      }
    } else {
      break()
    }
  }

  current
}

blobBGS <- function(x, background, mask = NULL, smoothing, threshold) {
  dif <- Rvision::gaussianBlur(Rvision::absdiff(x, background), 11, 11, smoothing, smoothing)

  if (Rvision::isImage(mask)) {
    dif <- dif * (mask / 255)
  }

  bin <- Rvision::changeColorSpace(dif, "GRAY") > threshold
  Rvision::connectedComponents(bin, 8, TRUE)
}

blobTracktor <- function(x, mask = NULL, smoothing, threshold) {
  blur <- Rvision::gaussianBlur(x, 11, 11, smoothing, smoothing)

  if (Rvision::isImage(mask)) {
    blur <- blur * (mask / 255)
  }

  gray <- Rvision::changeColorSpace(blur, "GRAY")
  bin <- Rvision::adaptiveThreshold(gray, C = threshold)
  Rvision::connectedComponents(bin, 8, TRUE)
}

pipeline <- function(video, begin, end, background = NULL, mask = NULL, smoothing,
                     threshold, minSize, maxSize, lookBack, maxDist, blobsizes,
                     progress = FALSE, display = FALSE, quality = 1, scale = 1) {

  n <- (end - begin + 1)
  tracks <- data.frame(id = numeric(n), x = numeric(n), y = numeric(n),
                       size = numeric(n), frame = numeric(n) - 2 * lookBack,
                       track = numeric(n))
  pos <- 0
  bs <- median(blobsizes)
  bs_thresh <- bs + 1.5 * IQR(blobsizes)

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
    if (Rvision::isImage(background)) {
      local_bg <- Rvision::resize(background, fx = quality, fy = quality, interpolation = "area")
    }
    if (Rvision::isImage(mask)) {
      local_mask <- Rvision::resize(mask, fx = quality, fy = quality, interpolation = "area")
    }
  } else {
    if (Rvision::isImage(background)) {
      local_bg <- Rvision::cloneImage(background)
    }
    if (Rvision::isImage(mask)) {
      local_mask <- Rvision::cloneImage(mask)
    }
  }

  Rvision::setProp(video, "POS_FRAMES", begin - 1)

  for (i in begin:end) {
    past <- dplyr::filter(tracks, frame > (i - lookBack) & frame < i) # REMOVE DPLYR DEPENDENCE HERE

    if (quality < 1) {
      frame <- Rvision::resize(Rvision::readNext(video), fx = quality, fy = quality,
                               interpolation = "area")
    } else {
      frame <- Rvision::readNext(video)
    }

    if (Rvision::isImage(local_bg)) {
      if (Rvision::isImage(local_mask)) {
        cc <- trackR:::blobBGS(frame, local_bg, local_mask, smoothing, threshold)
      } else {
        cc <- trackR:::blobBGS(frame, local_bg, NULL, smoothing, threshold)
      }

    } else {
      if (Rvision::isImage(local_mask)) {
        cc <- trackR:::blobTracktor(frame, local_mask, smoothing, threshold)
      } else {
        cc <- trackR:::blobTracktor(frame, NULL, smoothing, threshold)
      }
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
            clust <- kmeans(xy, k)$cluster

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
        # tmp <- trackR:::simpleTracker(blobs, past, lookBack = lookBack, maxDist = maxDist)
        tmp <- trackR:::simplerTracker(blobs, past, maxDist = maxDist)

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
        fps <- (i - oldFrame + 1) / as.numeric(difftime(newTime, oldTime, unit = "secs"))
        old <- new
        oldFrame <- i
        oldTime <- newTime
        pb$set(value = old / 100, detail = paste0(old, "% - ", round(fps, digits = 2), "fps"))
      }
    }

    if (display) {
      tmp <- dplyr::filter(tracks, track > 0 & frame > (i - fps(video)) & frame <= i) # REMOVE DPLYR DEPENDENCE HERE

      tmp %>% # REMOVE DPLYR DEPENDENCE HERE
        group_by(., track) %>%
        do(., a = if (length(.$x) > 1) {
          Rvision::drawLine(frame, .$x[1:(length(.$x) - 1)],
                   .$y[1:(length(.$x) - 1)],
                   .$x[2:length(.$x)],
                   .$y[2:length(.$x)], cbPalette[(.$track %% 12) + 1], round(ncol(frame) / 200))
          1
        } else {
          Rvision::drawCircle(frame, .$x, .$y, 2, .$track, 2)
          0
        }

        )

      Rvision::display(frame, "trackR", 1, nrow(frame) * scale / quality, ncol(frame) * scale / quality)
    }
  }

  tracks[1:pos, ]
}

