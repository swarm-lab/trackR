# Variables and reactives


# Outputs
output$blobStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (refreshDisplay() > -1 & !isImage(theBackground)) {
    p("Background missing (and required).", class = "bad")
  }
})

output$videoSlider3 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos3_x", "Frame", width = "100%", step = 1,
                value = frameMem,
                min = input$rangePos_x[1],
                max = input$rangePos_x[2])
  }
})


# Events
observeEvent(input$main, {
  if (input$main == "5") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(refreshDisplay(), {
  if (isVideo(theVideo) & input$blobWidth_x == 0) {
    updateNumericInput(session, "blobWidth_x", value = nrow(theVideo),
                       max = nrow(theVideo))
    updateNumericInput(session, "blobHeight_x", value = ncol(theVideo),
                       max = nrow(theVideo))
    updateNumericInput(session, "blobArea_x", max = nrow(theVideo) *
                         ncol(theVideo))
  }
})

observeEvent(input$blobHeight_x, {
  updateNumericInput(session, "blobWidth_x", max = input$blobHeight_x)
})

observeEvent(input$optimizeBlobs_x, {
  if (isVideo(theVideo) & isImage(theBackground)) {
    toggleAll("OFF")

    showNotification("Optimizing blob parameters.", id = "optim", duration = NULL)

    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2], length.out = 100))
    tot_summ <- NULL

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    background <- cloneImage(theBackground)

    if (!isImage(theMask)) {
      theMask <<- ones(nrow(theBackground), ncol(theBackground), 3)
      theMask %i*% 255
    }
    mask <- cloneImage(theMask)

    if (input$videoQuality_x < 1) {
      background <- resize(background, fx = input$videoQuality_x,
                           fy = input$videoQuality_x, interpolation = "area")
      mask <- resize(mask, fx = input$videoQuality_x,
                     fy = input$videoQuality_x, interpolation = "area")
    }

    if (input$darkButton_x == "Darker")
      not(background, target = "self")

    mask %i/% 255

    frame <- zeros(nrow(theBackground), ncol(theBackground), 3)
    proc_frame <- zeros(nrow(background), ncol(background), 3)
    cc_dump <- zeros(nrow(background), ncol(background), 1, "32S")

    for (i in 1:n) {
      frame <- readFrame(theVideo, frame_pos[i])

      if (input$videoQuality_x < 1) {
        resize(frame, fx = input$videoQuality_x, fy = input$videoQuality_x,
               interpolation = "area", target = proc_frame)
      } else {
        proc_frame <- frame
      }

      if (input$darkButton_x == "Darker")
        not(proc_frame, target = "self")

      proc_frame %i-% background
      proc_frame %i*% mask
      bw <- inRange(proc_frame, c(input$blueThreshold_x, input$greenThreshold_x,
                                  input$redThreshold_x, 0))
      boxFilter(bw, target = "self")
      bw %i>% 63

      nz <- as.data.table(connectedComponents(bw, 8, target = cc_dump)$table)
      nz <- nz[, if(.N >= 5) .SD, by = .(id)]
      nz_summ <- nz[, as.data.table(kbox(cbind(x, y))), by = .(id)]

      if (nrow(nz_summ) > 0) {
        nz_summ[, area := (width / 2) * (height / 2) * pi]
        nz_summ[, density := n / area]
        tot_summ <- rbindlist(list(tot_summ, nz_summ))
      }

      new_check <- floor(100 * i / n)
      if (new_check > (old_check + 5)) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time,
                                                         units = "secs"))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(value = new_check / 100,
               detail = paste0(new_check, "% - ",
                               round(fps, digits = 2), "fps"))
      }
    }

    tot_summ[, outlier := mvoutlier::pcout(
      cbind(scale(width), scale(height), scale(density)))$wfinal01 == 0]

    pb$close()

    removeNotification(id = "optim")
    toggleAll("ON")

    updateNumericInput(session, "blobWidth_x",
                       value = round(1.05 * max(tot_summ[outlier == FALSE, width])))
    updateNumericInput(session, "blobHeight_x",
                       value = round(1.05 * max(tot_summ[outlier == FALSE, height])))
    updateNumericInput(session, "blobArea_x",
                       value = round(0.95 * min(tot_summ[outlier == FALSE, n])))
    updateNumericInput(session, "blobDensity_x",
                       value = round(0.95 * min(tot_summ[outlier == FALSE, density]), 3))
  }
})

observeEvent(input$blobWidth_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$blobHeight_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$blobArea_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$blobDensity_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(refreshDisplay(), {
  if (input$main == "5") {
    if (!isImage(theImage) & !isImage(theBackground)) {
      toDisplay <- zeros(480, 640, 3)
    } else if (!isImage(theImage)) {
      toDisplay <- zeros(nrow(theBackground), ncol(theBackground), 3)
    } else if (!isImage(theBackground)) {
      toDisplay <- zeros(nrow(theImage), ncol(theImage), 3)
    } else {
      background <- cloneImage(theBackground)

      if (!isImage(theMask)) {
        theMask <<- ones(nrow(theBackground), ncol(theBackground), 3)
        theMask %i*% 255
      }
      mask <- cloneImage(theMask)

      if (input$videoQuality_x < 1) {
        background <- resize(background, fx = input$videoQuality_x,
                             fy = input$videoQuality_x, interpolation = "area")
        mask <- resize(mask, fx = input$videoQuality_x,
                       fy = input$videoQuality_x, interpolation = "area")
      }

      if (input$darkButton_x == "Darker")
        not(background, target = "self")

      mask %i/% 255

      frame <- cloneImage(theImage)
      proc_frame <- zeros(nrow(background), ncol(background), 3)
      cc_dump <- zeros(nrow(background), ncol(background), 1, "32S")

      if (input$videoQuality_x < 1) {
        proc_frame <- resize(frame, fx = input$videoQuality_x,
                             fy = input$videoQuality_x, interpolation = "area")
      } else {
        proc_frame <- frame
      }

      toDisplay <- cloneImage(proc_frame)

      if (input$darkButton_x == "Darker")
        not(proc_frame, target = "self")

      proc_frame %i-% background
      proc_frame %i*% mask

      bw <- inRange(proc_frame, c(input$blueThreshold_x, input$greenThreshold_x,
                                  input$redThreshold_x, 0))
      boxFilter(bw, target = "self")
      bw %i>% 63

      nz <- as.data.table(connectedComponents(bw, 8, target = cc_dump)$table)
      centers <- nz[, .(x = mean(x), y = mean(y)), by = .(id)]

      d <- Rfast::dista(nz[, 2:3], centers[, 2:3])
      nz[, k := Rfast::rowMins(d)]
      gr <- unique(nz[, .(id, k)])
      setorder(gr, id)
      gr[, new_id := id]

      for (j in 1:nrow(gr)) {
        friends <- gr$new_id[gr$k == gr$k[j]]
        gr$new_id[gr$new_id %in% friends] <- gr$new_id[j]
      }

      uid <- unique(gr$new_id)
      nz <- as.matrix(nz)
      gr <- as.matrix(gr)

      shape <- c()

      for (j in 1:length(uid)) {
        ix <- gr[, 3] == uid[j]
        ugr <- unique(gr[ix, 2])
        pos <- nz[nz[, 1] %in% gr[ix, 1], 2:3]
        cl <- kbox(pos, centers[ugr, 2:3, drop = FALSE], iter.max = 1000,
                   split = TRUE, split.width = input$blobWidth_x,
                   split.height = input$blobHeight_x,
                   split.density = input$blobDensity_x,
                   min.size = input$blobArea_x)
        shape <- rbind(shape, cl)
      }

      sc <- max(dim(toDisplay) / 720)

      avg_rgb <- mean(toDisplay) / 255
      avg_rgb <- avg_rgb == min(avg_rgb)
      color <- rgb(avg_rgb[3], avg_rgb[2], avg_rgb[1])

      if (length(shape) > 0) {
        drawRotatedRectangle(toDisplay, shape[, 1], shape[, 2], shape[, 3],
                             shape[, 4], shape[, 5], color = color,
                             thickness = max(1, 1.5 * sc))
      }

      x <- round(0.025 * ncol(toDisplay))
      y <- round(0.025 * nrow(toDisplay))
      avg_col <- mean(toDisplay[y:(y+50), x:(x+50),])

      drawLine(toDisplay, 0.025 * ncol(toDisplay), 0.025 * nrow(toDisplay),
               0.025 * ncol(toDisplay) + 50, 0.025 * nrow(toDisplay),
               color = color, thickness = max(1, 1.5 * sc))

      drawLine(toDisplay, 0.025 * ncol(toDisplay), 0.025 * nrow(toDisplay),
               0.025 * ncol(toDisplay), 0.025 * nrow(toDisplay) + 50,
               color = color, thickness = max(1, 1.5 * sc))

      drawText(toDisplay, "50 px", x + 6 * sc, y + 6 * sc, font_scale = max(0.5, 0.5 * sc),
               thickness = max(1, sc), color = color)
    }

    if (is.null(input$videoSize_x)) {
      display(toDisplay, "trackR", 5, nrow(toDisplay), ncol(toDisplay))
    } else {
      display(toDisplay, "trackR", 5,
              nrow(theImage) * input$videoSize_x,
              ncol(theImage) * input$videoSize_x)
    }
  }
})

observeEvent(input$videoPos3_x, {
  if (input$main == "5") {
    updateSliderInput(session, "videoPos_x", value = input$videoPos3_x)

    if (!is.null(input$videoPos2_x))
      updateSliderInput(session, "videoPos2_x", value = input$videoPos3_x)
  }

  refreshDisplay(refreshDisplay() + 1)
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeBlobs_x"))
