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

    target <- Rvision::readFrame(theVideo(), input$videoPos_x)
    target_gray <- Rvision::changeColorSpace(target, "GRAY")
    h_target <- Rvision::imhist(target)[, 1:target$nchan() + 1]
    cdf_target <- apply(h_target, 2, cumsum)
    map <- matrix(0, nrow = 256, ncol = target$nchan())

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

    frame <- Rvision::zeros(nrow(theVideo()), ncol(theVideo()), 3)
    frame_gray <- Rvision::zeros(nrow(theVideo()), ncol(theVideo()), 1)

    for (i in 1:n) {
      if (i == 1) {
        Rvision::readFrame(theVideo(), 1, frame)
      } else {
        Rvision::readNext(theVideo(), frame)
      }

      if (input$lightToggle_x) {
        h_frame <- Rvision::imhist(frame)[, 1:frame$nchan() + 1]
        cdf_frame <- apply(h_frame, 2, cumsum)

        for (j in 1:target$nchan()) {
          map[, j] <- apply(abs(outer(cdf_frame[, j], cdf_target[, j], "-")), 1, which.min) - 1
        }

        Rvision::LUT(frame, map, target = "self")
      }

      if (input$perspToggle_x) {
        Rvision::changeColorSpace(frame, "GRAY", target = frame_gray)
        tr <- Rvision::findTransformORB(target_gray, frame_gray, warp_mode = "homography")
        Rvision::warpPerspective(frame, tr, target = "self")
      }

      Rvision::writeFrame(vw, frame)

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
