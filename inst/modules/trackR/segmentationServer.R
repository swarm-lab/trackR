# Status
output$segmentationStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (!isImage(theBackground())) {
    p("Background missing (and required).", class = "bad")
  }
})

# Optimize thresholds
toOptim <- function(par, images) {
  out <- sapply(images, function(image) {
    bw <- inRange(image, c(par[1:3], 0))
    md <- medianBlur(bw, 1)
    md %i*% bw
    signal <- sum(md)
    noise <- sum(bw) - signal
    signal / (noise + 1)
  })

  sum(out)
}

observeEvent(input$optimizeThresholds_x, {
  if (isVideo(theVideo()) & isImage(theBackground())) {
    toggleAll("OFF")

    showNotification("Loading images in memory.", id = "load", duration = NULL)
    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2],
                               length.out = 20))# input$thresholdImages_x))

    background <- cloneImage(theBackground())
    mask <- cloneImage(theMask())

    if (input$videoQuality_x < 1) {
      background <- resize(background, fx = input$videoQuality_x,
                           fy = input$videoQuality_x,
                           interpolation = "area")
      mask <- resize(mask, fx = input$videoQuality_x,
                     fy = input$videoQuality_x,
                     interpolation = "area")
    }

    if (input$darkButton_x == "Darker")
      not(background)

    mask %i/% 255

    frames <- lapply(frame_pos, function(i) {
      frame <- readFrame(theVideo(), i)

      if (input$videoQuality_x < 1)
        frame <- resize(frame, fx = input$videoQuality_x,
                        fy = input$videoQuality_x,
                        interpolation = "area")

      if (input$darkButton_x == "Darker")
        not(frame)

      frame %i-% background
      frame %i*% mask
      frame
    })

    removeNotification(id = "load")
    showNotification("Optimizing thresholds. Please wait.", id = "optim", duration = NULL)

    d <- matrix(apply(sapply(frames, range), 1, max), ncol = 2, byrow = TRUE)
    d[, 1] <- d[, 1] + 1

    th <- rgenoud::genoud(toOptim, 3, max = TRUE, data.type.int = TRUE,
                          images = frames, Domains = d, pop.size = 25,
                          boundary.enforcement = 2, wait.generations	= 3,
                          print.level = 0)$par

    removeNotification(id = "optim")
    toggleAll("ON")

    updateSliderInput(session, "blueThreshold_x", value = th[1])
    updateSliderInput(session, "greenThreshold_x", value = th[2])
    updateSliderInput(session, "redThreshold_x", value = th[3])
  }
})


# Display segmented image
observe({
  if (input$main == "4") {
    if (is.null(input$videoSize_x)) {
      display(zeros(480, 640), "trackR", 5, 480, 640)
    } else {
      if (isImage(theBackground())) {
        background <- cloneImage(theBackground())
        mask <- cloneImage(theMask())

        if (input$videoQuality_x < 1) {
          background <- resize(background, fx = input$videoQuality_x,
                               fy = input$videoQuality_x,
                               interpolation = "area")
          mask <- resize(mask, fx = input$videoQuality_x,
                         fy = input$videoQuality_x,
                         interpolation = "area")
        }

        if (input$darkButton_x == "Darker")
          not(background)

        mask %i/% 255

        frame <- cloneImage(theImage())

        if (input$videoQuality_x < 1)
          frame <- resize(frame, fx = input$videoQuality_x,
                          fy = input$videoQuality_x,
                          interpolation = "area")

        if (input$darkButton_x == "Darker")
          not(frame)

        frame %i-% background
        frame %i*% mask

        sc <- max(dim(frame) / 720)

        bw <- inRange(frame, c(input$blueThreshold_x, input$greenThreshold_x,
                               input$redThreshold_x, 0))
        boxFilter(bw, in_place = TRUE)
        bw %i>% 63
        ct <- findContours(bw, method = "none")

        if (input$videoQuality_x < 1) {
          toDisplay <- resize(theImage(), fx = input$videoQuality_x,
                              fy = input$videoQuality_x, interpolation = "area")
        } else {
          toDisplay <- cloneImage(theImage())
        }
        drawCircle(toDisplay, ct$contours[, 2], ct$contours[, 3], sc, "green", -1)

        display(
          toDisplay,
          "trackR", 5,
          nrow(theImage()) * input$videoSize_x,
          ncol(theImage()) * input$videoSize_x)
      } else {
        display(zeros(480, 640), "trackR", 5, 480, 640)
      }
    }
  }
})


# Video slider
output$videoSlider2 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos2_x", "Frame", width = "100%", step = 1,
                value = frameMem,
                min = input$rangePos_x[1],
                max = input$rangePos_x[2])
  }
})

observeEvent(input$videoPos2_x, {
  if (input$main == "4") {
    updateSliderInput(session, "videoPos_x", value = input$videoPos2_x)

    if (!is.null(input$videoPos3_x))
      updateSliderInput(session, "videoPos3_x", value = input$videoPos2_x)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeThresholds_x"))
