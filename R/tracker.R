#' @export
findBlobs <- function(x, background, mask = NULL, threshold, minSize) {
  if (!is.null(mask)) {
    mask <- mask / 255
    x <- x * mask
    background <- background * mask
  }
  d <- gaussianBlur(background, sigma_x = 3, sigma_y = 3) -
    gaussianBlur(x, sigma_x = 3, sigma_y = 3)
  simpleBlobDetector(d, min_threshold = threshold, max_threshold = 255,
                     min_area = minSize, min_repeatability = 2,
                     min_dist_between_blobs = minSize, blob_color = 255,
                     filter_by_convexity = FALSE, filter_by_inertia = FALSE)
}

#' @export
pdiff <- function(a, b) {
  nr <- length(a)
  nc <- length(b)

  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)

  ma - mb
}

#' @export
simpleTracker <- function(current, past, lookBack = 30, maxDist = 10) {
  if (nrow(past) == 0) {
    current$track <- 1:nrow(current)
    return(current)
  }

  i <- current$frame[1]
  trackCounter <- max(past$track)
  # past <- dplyr::filter(past, frame > (i - lookBack), frame < i)

  mat <- abs(pdiff(current$x, past$x)) + abs(pdiff(current$y, past$y))
  maxMat <- matrix(maxDist * (i - past$frame), nrow = nrow(current),
                   ncol = nrow(past), byrow = TRUE)
  validMat <- mat <= maxMat

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
    newPast <- dplyr::filter(past, !(track %in% tracks[safe]))

    if (nrow(newCurrent) > nrow(newPast)) {
      n <- nrow(newCurrent) - nrow(newPast)
      newPast <- rbind(newPast, data.frame(id = NA, x = rep(-9999, n),
                                           y = rep(-9999, n), size = NA,
                                           frame = newCurrent$frame[1],
                                           track = NA))
    }

    mat <- abs(pdiff(newCurrent$x, newPast$x)) + abs(pdiff(newCurrent$y, newPast$y))
    maxMat <- matrix(maxDist * (i - newPast$frame), nrow = nrow(newCurrent),
                     ncol = nrow(newPast), byrow = TRUE)
    validMat <- mat <= maxMat

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

#' @export
pipeline <- function(video, begin, end, background, mask = NULL, threshold,
                     minSize, lookBack, maxDist, progress = FALSE,
                     display = FALSE, scale = 1) {

  n <- (end - begin + 1)
  tracks <- data.frame(id = numeric(n), x = numeric(n), y = numeric(n),
                       size = numeric(n), frame = numeric(n) - 2 * lookBack,
                       track = numeric(n))
  pos <- 0

  if (progress) {
    pb <- shiny::Progress$new()
    on.exit(pb$close())
    pb$set(message = "Computing tracks", value = 0, detail = "0%")
    nFrames <- length(begin:end)
    old <- 0
    oldFrame <- begin
    oldTime <- Sys.time()
  }

  for (i in begin:end) {
    past <- dplyr::filter(tracks, frame > (i - lookBack) & frame < i)

    frame <- readFrame(video, i)

    tmp <- frame %>%
      findBlobs(., background, mask, threshold, minSize) %>%
      dplyr::mutate(frame = i, track = NA) %>%
      {if (length(.$x) > 0)
        trackR::simpleTracker(., past, lookBack = lookBack, maxDist = maxDist)
        else . }

    nRows <- nrow(tmp)
    if (nRows > 0) {
      if ((pos + nRows) > n) {
        tracks[(n + 1):(2 * n + nRows), ] <- 0
        n <- nrow(tracks)
      }
      tracks[(pos + 1):(pos + nRows), ] <- tmp
      pos <- pos + nRows
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
      tmp <- dplyr::filter(tracks, frame > (i - 2 * fps(video)) & frame <= i)

      tmp %>%
        group_by(., track) %>%
        do(., a = if (length(.$x) > 1) {
          drawLine(frame, .$x[1:(length(.$x) - 1)],
                   .$y[1:(length(.$x) - 1)],
                   .$x[2:length(.$x)],
                   .$y[2:length(.$x)], .$track, 2)
          1
        } else {
          drawCircle(frame, .$x, .$y, 2, .$track , 2)
          0
        }

        )

      display(frame, "trackR", 1, nrow(frame) * scale, ncol(frame) * scale)
    }
  }

  tracks[1:pos, ]
}
