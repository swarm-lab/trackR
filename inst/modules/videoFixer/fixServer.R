theFixedVideoPath <- reactiveVal()

# Toggle UI on and off during long operations
toggleAll <- function(state = "OFF") {
  input_list <- reactiveValuesToList(input)
  to_toggle <- grepl("_x", names(input_list))
  input_list <- input_list[to_toggle]

  for(name in names(input_list)) {
    if (state == "OFF") {
      shinyjs::disable(name)
    } else {
      shinyjs::enable(name)
    }
  }
}

# Enable panel
observe({
  if (Rvision::isVideo(theVideo())) {
    enable(selector = "a[data-value=2]")
  }
})

# Controls
observe({
  if (Rvision::isVideo(theVideo())) {
    updateSliderInput(session, "refWidth_x", value = ncol(theVideo()),
                      max = ncol(theVideo()))
    updateSliderInput(session, "refHeight_x", value = nrow(theVideo()),
                      max = nrow(theVideo()))
  }
})

observeEvent(input$refWidth_x, {
  if (Rvision::isVideo(theVideo())) {
    updateSliderInput(session, "refShiftLeft_x", max = ncol(theVideo()) - input$refWidth_x)
  }
})

observeEvent(input$refHeight_x, {
  if (Rvision::isVideo(theVideo())) {
    updateSliderInput(session, "refShiftUp_x", max = nrow(theVideo()) - input$refHeight_x)
  }
})

# Display video
observeEvent(input$refWidth_x, {
  redraw(redraw() + 1)
})

observeEvent(input$refHeight_x, {
  redraw(redraw() + 1)
})

observeEvent(input$refShiftLeft_x, {
  redraw(redraw() + 1)
})

observeEvent(input$refShiftUp_x, {
  redraw(redraw() + 1)
})

observeEvent(input$main, {
  if (input$main == "2") {
    redraw(redraw() + 1)
  }
})

observeEvent(redraw(), {
  if (Rvision::isImage(theImage())) {
    toDisplay <- Rvision::cloneImage(theImage())
    Rvision::drawRectangle(toDisplay, color = "green", thickness = 5,
                           input$refShiftLeft_x + 1, input$refShiftUp_x + 1,
                           input$refWidth_x + input$refShiftLeft_x,
                           input$refHeight_x + input$refShiftUp_x)
    Rvision::display(toDisplay, "videoFixer", 5,
                     nrow(theImage()) * input$videoSize_x,
                     ncol(theImage()) * input$videoSize_x)
  } else {
    Rvision::display(Rvision::zeros(480, 640), "videoFixer", 5, 480, 640)
  }
})


# Export video
shinyFileSave(input, "exportVideo_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$exportVideo_x, {
  path <- parseSavePath(volumes, input$exportVideo_x)
  theFixedVideoPath(path$datapath)
})

observeEvent(theFixedVideoPath(), {
  if (length(theFixedVideoPath()) > 0) {
    toggleAll("OFF")

    showNotification("Exporting video.", id = "exporting", duration = NULL)

    n <- diff(input$rangePos_x) + 1

    target_raw <- Rvision::readFrame(theVideo(), input$videoPos_x)
    target_color <- Rvision::subImage(target_raw, input$refShiftLeft_x + 1,
                                input$refShiftUp_x + 1, input$refWidth_x,
                                input$refHeight_x)
    target_gray <- Rvision::changeColorSpace(target_color, "GRAY")

    h_target <- Rvision::imhist(target_color)[, 1:target_color$nchan() + 1]
    cdf_target <- apply(h_target, 2, cumsum)
    map <- matrix(0, nrow = 256, ncol = target_color$nchan())

    vw <- Rvision::videoWriter(theFixedVideoPath(),
                               fourcc = "avc1",
                               fps = theVideo()$fps(),
                               height = theVideo()$nrow(),
                               width = theVideo()$ncol())

    pb <- Progress$new()
    pb$set(message = "Processing: ", value = 0, detail = "0%")
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    frame_raw <- Rvision::zeros(theVideo()$nrow(), theVideo()$ncol(), 3)
    frame_color <- Rvision::zeros(target_color$nrow(), target_color$ncol(), 3)
    frame_gray <- Rvision::zeros(target_gray$nrow(), target_gray$ncol(), 1)

    for (i in 1:n) {
      if (i == 1) {
        Rvision::readFrame(theVideo(), 1, frame_raw)
      } else {
        Rvision::readNext(theVideo(), frame_raw)
      }

      Rvision::subImage(frame_raw, input$refShiftLeft_x + 1, input$refShiftUp_x + 1,
                        input$refWidth_x, input$refHeight_x, target = frame_color)

      if (input$perspToggle_x) {
        Rvision::changeColorSpace(frame_color, "GRAY", target = frame_gray)

        if (input$perspSpeed_x == "Faster") {
          tr <- Rvision::findTransformORB(target_gray, frame_gray, warp_mode = "affine")
        } else {
          tr <- Rvision::findTransformECC(target_gray, frame_gray, warp_mode = "euclidean")
        }

        Rvision::warpAffine(frame_raw, tr, target = "self")
      }

      if (input$lightToggle_x) {
        Rvision::subImage(frame_raw, input$refShiftLeft_x + 1, input$refShiftUp_x + 1,
                          input$refWidth_x, input$refHeight_x, target = frame_color)
        h_frame <- Rvision::imhist(frame_color)[, 1:frame_color$nchan() + 1]
        cdf_frame <- apply(h_frame, 2, cumsum)

        for (j in 1:target_color$nchan()) {
          map[, j] <- apply(abs(outer(cdf_frame[, j], cdf_target[, j], "-")), 1, which.min) - 1
        }

        Rvision::LUT(frame_raw, map, target = "self")
      }

      Rvision::writeFrame(vw, frame_raw)

      new_check <- floor(100 * i / n)
      if (new_check > old_check) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(value = new_check / 100, detail = paste0(new_check, "% - ", round(fps, digits = 2), "fps"))
      }
    }

    Rvision::release(vw)

    pb$close()
    removeNotification(id = "exporting")
    toggleAll("ON")
  }
})
