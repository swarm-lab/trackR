# Status
output$blobStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (!Rvision::isImage(theBackground())) {
    p("Background missing (and required).", class = "bad")
  }
})

# Split parameters
observeEvent(theVideo(), {
  if (Rvision::isVideo(theVideo())) {
    if (input$blobWidth_x == 0) {
      updateNumericInput(session, "blobWidth_x", value = nrow(theVideo()),
                         max = nrow(theVideo()))
      updateNumericInput(session, "blobHeight_x", value = ncol(theVideo()),
                         max = nrow(theVideo()))
      updateNumericInput(session, "blobArea_x", max = nrow(theVideo()) *
                           ncol(theVideo()))
    }
  }
})

observeEvent(input$blobHeight_x, {
  updateNumericInput(session, "blobWidth_x", max = input$blobHeight_x)
})

observeEvent(input$optimizeBlobs_x, {
  if (Rvision::isVideo(theVideo()) & Rvision::isImage(theBackground())) {
    toggleAll("OFF")

    showNotification("Optimizing blob parameters.", id = "optim", duration = NULL)

    frame_pos <- round(seq.int(input$rangePos_x[1],
                               input$rangePos_x[2],
                               length.out = 100)) # input$blobImages_x))
    tot_summ <- NULL

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    for (i in 1:n) {
      frame <- Rvision::readFrame(theVideo(), frame_pos[i])

      if (input$darkButton_x == "Darker") {
        d <- Rvision::resize((theBackground() - frame) * (theMask() / 255),
                             fx = input$videoQuality_x, fy = input$videoQuality_x,
                             interpolation = "area")
      } else {
        d <- Rvision::resize((frame - theBackground()) * (theMask() / 255),
                             fx = input$videoQuality_x, fy = input$videoQuality_x,
                             interpolation = "area")
      }

      bw <- Rvision::inRange(d, c(input$blueThreshold_x, input$greenThreshold_x,
                                  input$redThreshold_x, 0))
      Rvision::boxFilter(bw, in_place = TRUE)
      nz <- data.table::as.data.table(Rvision::connectedComponents(bw > 63, 8)$table)
      nz <- nz[, if(.N >= 5) .SD, by = .(id)]
      nz_summ <- nz[, data.table::as.data.table(kbox(cbind(x, y))), by = .(id)]

      if (nrow(nz_summ) > 0) {
        nz_summ[, area := (width / 2) * (height / 2) * pi]
        nz_summ[, density := n / area]
        tot_summ <- data.table::rbindlist(list(tot_summ, nz_summ))
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

# Display blobs
observe({
  if (input$main == "5") {
    if (is.null(input$videoSize_x)) {
      Rvision::display(Rvision::zeros(480, 640), "trackR", 25, 480, 640)
    } else {
      if (Rvision::isImage(theBackground())) {
        if (input$darkButton_x == "Darker") {
          d <- Rvision::resize((theBackground() - theImage()) * (theMask() / 255),
                               fx = input$videoQuality_x, fy = input$videoQuality_x,
                               interpolation = "area")
        } else {
          d <- Rvision::resize((theImage() - theBackground()) * (theMask() / 255),
                               fx = input$videoQuality_x, fy = input$videoQuality_x,
                               interpolation = "area")
        }

        sc <- max(dim(d) / 720)

        bw <- Rvision::boxFilter(
          Rvision::inRange(d, c(input$blueThreshold_x,
                                input$greenThreshold_x,
                                input$redThreshold_x, 0))
        ) > 63

        nz <- data.table::as.data.table(Rvision::connectedComponents(bw > 63, 8)$table)
        centers <- nz[, .(x = mean(x), y = mean(y)), by = .(id)]

        d <- Rfast::dista(nz[, 2:3], centers[, 2:3])
        nz[, k := Rfast::rowMins(d)]
        gr <- nz[, .N, by = .(id, k)]
        data.table::setorder(gr, id)
        gr[, new_id := id]
        gr <- gr[N > input$blobArea_x, ]

        for (j in 1:nrow(gr)) {
          friends <- gr$new_id[gr$k == gr$k[j]]
          gr$new_id[gr$new_id %in% friends] <- gr$new_id[j]
        }

        uid <- unique(gr$new_id)
        nz <- as.matrix(nz)
        gr <- as.matrix(gr)

        shape <- c()

        for (j in 1:length(uid)) {
          ix <- gr[, 4] == uid[j]
          ugr <- unique(gr[ix, 2])
          pos <- nz[nz[, 1] %in% gr[ix, 1], 2:3]
          cl <- kbox(pos, centers[ugr, 2:3, drop = FALSE], iter.max = 1000,
                     split = TRUE, split.width = input$blobWidth_x,
                     split.height = input$blobHeight_x,
                     split.density = input$blobDensity_x,
                     min.size = input$blobArea_x)
          shape <- rbind(shape, cl)
        }

        toDisplay <- Rvision::resize(theImage(), fx = input$videoQuality_x,
                                    fy = input$videoQuality_x, interpolation = "area")
        if (length(shape) > 0) {
          Rvision::drawRotatedRectangle(toDisplay, shape[, 1], shape[, 2],
                                        shape[, 3], shape[, 4],
                                        shape[, 5], color = "green",
                                        thickness = 1.5 * sc)
        }

        Rvision::display(
          toDisplay,
          "trackR", 25,
          nrow(toDisplay) * input$videoSize_x,
          ncol(toDisplay) * input$videoSize_x)
      } else {
        Rvision::display(Rvision::zeros(480, 640), "trackR", 25, 480, 640)
      }
    }
  }
})


# Video slider
output$videoSlider3 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos3_x", "Frame", width = "100%", step = 1,
                value = frameMem,
                min = input$rangePos_x[1],
                max = input$rangePos_x[2])
  }
})

observeEvent(input$videoPos3_x, {
  if (input$main == "5") {
    updateSliderInput(session, "videoPos_x", value = input$videoPos3_x)

    if (!is.null(input$videoPos2_x))
      updateSliderInput(session, "videoPos2_x", value = input$videoPos3_x)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeBlobs_x"))
