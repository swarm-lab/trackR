#--------------------------------------------------------------
# Status Outputs
#--------------------------------------------------------------
observeEvent(print_display(), {
  if (is.null(input$video_pos2_x)) {
    toggleTabs(5:6, "OFF")
  } else {
    toggleTabs(5, "ON")
  }
})


#--------------------------------------------------------------
# UI Outputs
#--------------------------------------------------------------
output$video_slider2 <- renderUI({
  if (!is.null(input$range_pos_x)) {
    sliderInput("video_pos2_x", "Frame",
      width = "100%", step = 1,
      value = frame_mem,
      min = input$range_pos_x[1],
      max = input$range_pos_x[2]
    )
  }
})


#--------------------------------------------------------------
# Events
#--------------------------------------------------------------
observeEvent(input$main, {
  if (input$main == "4") {
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$optimize_thresholds_x, {
  if (isVideoStack(the_video) & isImage(the_background) &
    nrow(the_image) == nrow(the_background) &
    ncol(the_image) == ncol(the_background)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")

    showNotification("Loading images in memory.", id = "load", duration = NULL)

    frame_pos <- round(seq.int(input$range_pos_x[1], input$range_pos_x[2],
      length.out = 20
    ))

    background <- cloneImage(the_background)
    changeColorSpace(background, "GRAY", "self")

    if (!isImage(the_mask) | nrow(the_image) != nrow(the_mask) |
      ncol(the_image) != ncol(the_mask)) {
      mask <- ones(nrow(the_background), ncol(the_background), 3)
    } else {
      mask <- cloneImage(the_mask)
    }
    changeColorSpace(mask, "GRAY", "self")

    if (input$dark_button_x == "Darker") {
      not(background, "self")
    }

    frames <- lapply(frame_pos, function(i) {
      frame <- readFrame(the_video, i)
      changeColorSpace(frame, "GRAY", "self")

      if (input$dark_button_x == "Darker") {
        not(frame, target = "self")
      }

      if (input$dark_button_x == "A bit of both") {
        absdiff(frame, background, "self")
      } else {
        frame %i-% background
      }

      mask %i>% 0
      mask %i/% 255
      frame %i*% mask
      frame
    })

    removeNotification(id = "load")
    showNotification("Optimizing threshold. Please wait.",
      id = "optim", duration = NULL
    )

    th <- as.integer(
      mean(
        sapply(frames, function(f) {
          autothreshold(f, method = input$threshold_method_x)
        })
      )
    )

    removeNotification(id = "optim")
    toggleInputs("ON")
    toggleTabs(1:6, "ON")

    updateSliderInput(session, "threshold_x", value = th[1])
  }
})

observeEvent(input$blueThreshold_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(input$greenThreshold_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(input$redThreshold_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(input$dark_button_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(refresh_display(), {
  if (input$main == "4") {
    if (!isImage(the_image) & !isImage(the_background)) {
      suppressMessages(
        write.Image(
          zeros(1080, 1920, 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else if (!isImage(the_image)) {
      suppressMessages(
        write.Image(
          zeros(nrow(the_background), ncol(the_background), 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else if (!isImage(the_background)) {
      suppressMessages(
        write.Image(
          zeros(nrow(the_image), ncol(the_image), 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else if (nrow(the_image) != nrow(the_background) |
      ncol(the_image) != ncol(the_background)) {
      suppressMessages(
        write.Image(
          zeros(nrow(the_image), ncol(the_image), 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else {
      background <- cloneImage(the_background)
      changeColorSpace(background, "GRAY", "self")

      if (!isImage(the_mask) | nrow(the_image) != nrow(the_mask) |
        ncol(the_image) != ncol(the_mask)) {
        mask <- ones(nrow(the_background), ncol(the_background), 1)
      } else {
        mask <- cloneImage(the_mask)
      }
      changeColorSpace(mask, "GRAY", "self")

      if (input$dark_button_x == "Darker") {
        not(background, target = "self")
      }

      image <- cloneImage(the_image)
      changeColorSpace(image, "GRAY", "self")

      if (input$dark_button_x == "Darker") {
        not(image, target = "self")
      }

      if (input$dark_button_x == "A bit of both") {
        absdiff(image, background, "self")
      } else {
        image %i-% background
      }

      mask %i>% 0
      mask %i/% 255
      image %i*% mask

      to_display <- cloneImage(image * (255 / max(max(image))))
      changeColorSpace(to_display, "BGR", "self")
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
        write.Image(to_display, paste0(tmp_dir, "/display.jpg"), TRUE)
      )
    }

    print_display(print_display() + 1)
  }
})

observeEvent(input$video_pos2_x, {
  if (input$main == "4") {
    updateSliderInput(session, "video_pos_x", value = input$video_pos2_x)

    if (!is.null(input$video_pos3_x)) {
      updateSliderInput(session, "video_pos3_x", value = input$video_pos2_x)
    }
  }

  refresh_display(refresh_display() + 1)
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(session$getBookmarkExclude(), "optimize_thresholds_x"))
