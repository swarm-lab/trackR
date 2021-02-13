# Status
output$segmentationStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (!Rvision::isImage(theBackground())) {
    p("Background missing (and required).", class = "bad")
  }
})

# Optimize thresholds
toOptim <- function(par, images) {
  out <- sapply(images, function(image) {
    bw <- Rvision::inRange(image, c(par[1:3], 0))
    md <- Rvision::medianBlur(bw, 1)
    signal <- Rvision::sum(md * bw)
    noise <- Rvision::sum(bw) - signal
    signal / (noise + 1)
  })

  sum(out)
}

observeEvent(input$optimizeThresholds_x, {
  if (Rvision::isVideo(theVideo()) & Rvision::isImage(theBackground())) {
    toggleAll("OFF")

    showNotification("Loading images in memory.", id = "load", duration = NULL)
    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2],
                               length.out = input$thresholdImages_x))

    m_bg <- Rvision::sum(theBackground() * (theMask() / 255))

    frames <- lapply(frame_pos, function(i) {
      frame <- Rvision::readFrame(theVideo(), i)
      m_fr <- Rvision::sum(frame * (theMask() / 255))
      r <- mean(m_bg / m_fr)

      if (input$darkButton_x == "Darker") {
        (theBackground() - (frame * r)) * (theMask() / 255)
      } else {
        ((frame * r) - theBackground()) * (theMask() / 255)
      }
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
      Rvision::display(Rvision::zeros(480, 640), "trackR", 25, 480, 640)
    } else {
      if (Rvision::isImage(theBackground())) {
        m_bg <- Rvision::sum(theBackground() * (theMask() / 255))
        m_fr <- Rvision::sum(theImage() * (theMask() / 255))
        r <- mean(m_bg / m_fr)
        if (input$darkButton_x == "Darker") {
          d <- (theBackground() - (theImage() * r)) * (theMask() / 255)
        } else {
          d <- ((theImage() * r) - theBackground()) * (theMask() / 255)
        }

        bw <- Rvision::inRange(d, c(input$blueThreshold_x, input$greenThreshold_x,
                                    input$redThreshold_x, 0))
        ct <- Rvision::findContours(Rvision::boxFilter(bw) > 63, method = "none")
        toDisplay <- Rvision::cloneImage(theImage())
        Rvision::drawCircle(toDisplay, ct$contours$x, ct$contours$y, 2, "green", -1)

        Rvision::display(
          Rvision::resize(toDisplay, fx = input$videoQuality_x,
                          fy = input$videoQuality_x, interpolation = "area"),
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
output$videoSlider2 <- renderUI({
  if (Rvision::isVideo(theVideo()) & !is.null(input$rangePos_x) &
      !is.null(input$videoPos_x)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      sliderInput("videoPos2_x", "Frame", width = "100%", value = input$videoPos_x,
                  min = 1, max = diff(input$rangePos_x) + 1, step = 1)
    } else {
      sliderInput("videoPos2_x", "Frame", width = "100%", value = input$videoPos_x,
                  min = 1, max = diff(input$rangePos_x) + 1, step = 1)
    }
  }
})

observe({
  if (Rvision::isVideo(theVideo()) & !is.null(input$videoPos2_x) &
      !is.null(input$rangePos_x) & !is.null(input$videoQuality_x)) {
    isolate({
      updateSliderInput(session, inputId = "videoPos_x", value = input$videoPos2_x)
    })
  }
})

# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeThresholds_x"))