# Variables and reactives
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


# Outputs
output$segmentationStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (refreshDisplay() > -1 & !isImage(theBackground)) {
    p("Background missing (and required).", class = "bad")
  }
})

output$videoSlider2 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos2_x", "Frame", width = "100%", step = 1,
                value = frameMem,
                min = input$rangePos_x[1],
                max = input$rangePos_x[2])
  }
})


# Events
observeEvent(input$main, {
  if (input$main == "4") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$optimizeThresholds_x, {
  if (isVideo(theVideo) & isImage(theBackground)) {
    toggleAll("OFF")

    showNotification("Loading images in memory.", id = "load", duration = NULL)

    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2], length.out = 20))

    background <- cloneImage(theBackground)

    if (!isImage(theMask)) {
      theMask <<- ones(nrow(theBackground), ncol(theBackground), 1)
      theMask %i*% 255
    }
    mask <- cloneImage(theMask)

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
      frame <- readFrame(theVideo, i)

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

observeEvent(input$blueThreshold_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$greenThreshold_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$redThreshold_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$darkButton_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(refreshDisplay(), {
  if (input$main == "4") {
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
        mask <- resize(mask, fx = input$videoQuality_x, fy = input$videoQuality_x,
                       interpolation = "area")
      }

      if (input$darkButton_x == "Darker")
        not(background, target = "self")

      mask %i/% 255

      image <- cloneImage(theImage)

      if (input$videoQuality_x < 1)
        image <- resize(image, fx = input$videoQuality_x,
                        fy = input$videoQuality_x, interpolation = "area")

      toDisplay <- cloneImage(image)

      if (input$darkButton_x == "Darker")
        not(image, target = "self")

      image %i-% background
      image %i*% mask

      sc <- max(dim(image) / 720)

      bw <- inRange(image, c(input$blueThreshold_x, input$greenThreshold_x,
                             input$redThreshold_x, 0))
      boxFilter(bw, target = "self")
      bw %i>% 63
      ct <- findContours(bw, method = "none")

      avg_rgb <- mean(theImage) / 255
      avg_rgb <- avg_rgb == min(avg_rgb)
      color <- rgb(avg_rgb[3], avg_rgb[2], avg_rgb[1])

      drawCircle(toDisplay, ct$contours[, 2], ct$contours[, 3], max(0.5, sc), color, -1)

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

observeEvent(input$videoPos2_x, {
  if (input$main == "4") {
    updateSliderInput(session, "videoPos_x", value = input$videoPos2_x)

    if (!is.null(input$videoPos3_x))
      updateSliderInput(session, "videoPos3_x", value = input$videoPos2_x)
  }

  refreshDisplay(refreshDisplay() + 1)
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeThresholds_x"))
