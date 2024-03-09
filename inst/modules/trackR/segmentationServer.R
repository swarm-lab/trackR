# Variables and reactives


# Status
observeEvent(printDisplay(), {
  if (is.null(input$videoPos2_x)) {
    toggleTabs(5:6, "OFF")
  } else {
    toggleTabs(5, "ON")
  }
})


# UI
output$videoSlider2 <- renderUI({
  if (!is.null(input$rangePos_x)) {
    sliderInput("videoPos2_x", "Frame",
      width = "100%", step = 1,
      value = frameMem,
      min = input$rangePos_x[1],
      max = input$rangePos_x[2]
    )
  }
})


# Events
observeEvent(input$main, {
  if (input$main == "4") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$optimizeThresholds_x, {
  if (isVideoStack(theVideo) & isImage(theBackground) &
    nrow(theImage) == nrow(theBackground) &
    ncol(theImage) == ncol(theBackground)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")

    showNotification("Loading images in memory.", id = "load", duration = NULL)

    frame_pos <- round(seq.int(input$rangePos_x[1], input$rangePos_x[2], 
      length.out = 20))

    background <- cloneImage(theBackground)
    changeColorSpace(background, "GRAY", "self")

    if (!isImage(theMask) | nrow(theImage) != nrow(theMask) |
      ncol(theImage) != ncol(theMask)) {
      mask <- ones(nrow(theBackground), ncol(theBackground), 3)
    } else {
      mask <- cloneImage(theMask)
    }
    changeColorSpace(mask, "GRAY", "self")

    if (input$darkButton_x == "Darker") {
      not(background, "self")
    }

    frames <- lapply(frame_pos, function(i) {
      frame <- readFrame(theVideo, i)
      changeColorSpace(frame, "GRAY", "self")

      if (input$darkButton_x == "Darker") {
        not(frame, target = "self")
      }

      if (input$darkButton_x == "A bit of both") {
        absdiff(frame, background, "self")
      } else {
        frame %i-% background
      }

      mask %i>% 0
      mask %i/% 255
      frame %i*% mask
      # split(frame)
      frame
    })

    removeNotification(id = "load")
    showNotification("Optimizing threshold. Please wait.",
      id = "optim", duration = NULL
    )

    th <- as.integer(
      mean(
        sapply(frames, function(f) {
          # sapply(f, autothreshold, method = input$thresholdMethod_x)
          autothreshold(f, method = input$thresholdMethod_x)
        })
      )
    )

    removeNotification(id = "optim")
    toggleInputs("ON")
    toggleTabs(1:6, "ON")

    updateSliderInput(session, "threshold_x", value = th[1])
    # updateSliderInput(session, "blueThreshold_x", value = th[1])
    # updateSliderInput(session, "greenThreshold_x", value = th[2])
    # updateSliderInput(session, "redThreshold_x", value = th[3])
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
      suppressMessages(
        write.Image(
          zeros(1080, 1920, 3),
          paste0(tmpDir, "/display.jpg"), TRUE
        )
      )
    } else if (!isImage(theImage)) {
      suppressMessages(
        write.Image(
          zeros(nrow(theBackground), ncol(theBackground), 3),
          paste0(tmpDir, "/display.jpg"), TRUE
        )
      )
    } else if (!isImage(theBackground)) {
      suppressMessages(
        write.Image(
          zeros(nrow(theImage), ncol(theImage), 3),
          paste0(tmpDir, "/display.jpg"), TRUE
        )
      )
    } else if (nrow(theImage) != nrow(theBackground) |
      ncol(theImage) != ncol(theBackground)) {
      suppressMessages(
        write.Image(
          zeros(nrow(theImage), ncol(theImage), 3),
          paste0(tmpDir, "/display.jpg"), TRUE
        )
      )
    } else {
      background <- cloneImage(theBackground)
      changeColorSpace(background, "GRAY", "self")

      if (!isImage(theMask) | nrow(theImage) != nrow(theMask) |
        ncol(theImage) != ncol(theMask)) {
        mask <- ones(nrow(theBackground), ncol(theBackground), 1)
      } else {
        mask <- cloneImage(theMask)
      }
      changeColorSpace(mask, "GRAY", "self")

      if (input$darkButton_x == "Darker") {
        not(background, target = "self")
      }

      image <- cloneImage(theImage)
      changeColorSpace(image, "GRAY", "self")

      if (input$darkButton_x == "Darker") {
        not(image, target = "self")
      }

      if (input$darkButton_x == "A bit of both") {
        absdiff(image, background, "self")
      } else {
        image %i-% background
      }

      mask %i>% 0
      mask %i/% 255
      image %i*% mask

      to_display <- cloneImage(image * (255 / max(max(image))))
      changeColorSpace(to_display, "BGR", "self")

      # bw <- inRange(image, c(
      #   input$blueThreshold_x, input$greenThreshold_x,
      #   input$redThreshold_x, 0
      # ))
      bw <- image >= input$threshold_x
      boxFilter(bw, 1, 1, target = "self")
      bw %i>% 63
      ct <- findContours(bw, method = "none")

      sc <- max(dim(image) / 720)
      color <- "green"
      drawCircle(
        to_display, ct$contours[, 2], ct$contours[, 3],
        max(0.5, sc), color, -1
      )
      suppressMessages(
        write.Image(to_display, paste0(tmpDir, "/display.jpg"), TRUE)
      )
    }

    printDisplay(printDisplay() + 1)
  }
})

observeEvent(input$videoPos2_x, {
  if (input$main == "4") {
    updateSliderInput(session, "videoPos_x", value = input$videoPos2_x)

    if (!is.null(input$videoPos3_x)) {
      updateSliderInput(session, "videoPos3_x", value = input$videoPos2_x)
    }
  }

  refreshDisplay(refreshDisplay() + 1)
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "optimizeThresholds_x"))
