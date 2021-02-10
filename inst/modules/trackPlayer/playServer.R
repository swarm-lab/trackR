theTrackVideoPath <- reactiveVal()

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
  if (Rvision::isVideo(theVideo()) & is.data.frame(theTracks())) {
    enable(selector = "a[data-value=2]")
    updateVerticalTabsetPanel(session, "main", selected = "2")
  }
})

# Status
output$videoStatus2 <- renderUI({
  if (!Rvision::isVideo(theVideo())) {
    p("Video missing (and required).", class = "bad")
  }
})

output$trackStatus2 <- renderUI({
  if (!is.data.frame(theTracks())) {
    p("Tracks missing (and required).", class = "bad")
  }
})

# Display slider
output$displaySlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

# Video slider
output$videoSlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    if (is.data.frame(theTracks())) {
      sliderInput("videoPos_x", NULL, width = "100%",
                  value = min(theTracks()[, frame]), min = 1,
                  max = Rvision::nframes(theVideo()), step = 1)
    } else {
      sliderInput("videoPos_x", NULL, width = "100%", value = 1, min = 1,
                  max = Rvision::nframes(theVideo()), step = 1)
    }
  }
})

# Controls
play <- reactiveVal(FALSE)

observeEvent(input$playPause_x, {
  if (!play() & Rvision::isVideo(theVideo())) {
    play(TRUE)
  } else {
    play(FALSE)
  }
})

observeEvent(input$videoSize_x, {
  theImage(Rvision::readFrame(theVideo(), input$videoPos_x))
})

observeEvent(input$videoPos_x, {
  if (input$videoPos_x - theVideo()$frame() == 1) {
    theImage(Rvision::readNext(theVideo()))
  } else {
    theImage(Rvision::readFrame(theVideo(), input$videoPos_x))
  }
})

observe({
  if (play()) {
    updateSliderInput(session, "videoPos_x", value = input$videoPos_x + 1)
  }
})

observeEvent(input$minusFrame_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x - 1)
})

observeEvent(input$plusFrame_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x + 1)
})

observeEvent(input$minusSec_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x - theVideo()$fps())
})

observeEvent(input$plusSec_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x + theVideo()$fps())
})

# Display video
refreshDisplay <- reactiveVal(0)

observe({
  if (Rvision::isImage(theImage()) & is.data.frame(theTracks())) {
    isolate( refreshDisplay(refreshDisplay() + 1) )
  }
})

observeEvent(refreshDisplay(), {
  if (refreshDisplay() > 0) {
    tmp_rect <- theTracks()[ignore == FALSE & frame == input$videoPos_x, ]

    if (nrow(tmp_rect) > 0) {
      sc <- max(dim(theImage()) / 720)

      overlay1 <- Rvision::cloneImage(theImage())
      Rvision::drawRotatedRectangle(overlay1, tmp_rect$x, tmp_rect$y,
                                    tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                    color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                    thickness = 1.5 * sc)

      overlay2 <- Rvision::cloneImage(theImage())
      Rvision::drawRotatedRectangle(overlay2, tmp_rect$x, tmp_rect$y,
                                    tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                    color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                    thickness = -1)

      tmp_tracks <- theTracks()[ignore == FALSE &
                                  frame >= (input$videoPos_x - 1 * theVideo()$fps()) &
                                  frame <= input$videoPos_x, ]

      tmp_tracks[, Rvision::drawPolyline(overlay2, cbind(x, y), FALSE,
                                         color = cbPalette[(track_fixed[1] %% 12) + 1],
                                         thickness = 3 * sc),
                 by = track_fixed]

      to_display <- Rvision::addWeighted(overlay1, overlay2, c(0.5, 0.5))
      Rvision::drawText(to_display, tmp_rect$track_fixed,
                        tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                        tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                        color = "white")

      Rvision::display(to_display, "trackR", 5,
                       nrow(to_display) * input$videoSize_x,
                       ncol(to_display) * input$videoSize_x)
    } else {
      Rvision::display(theImage(), "trackR", 5,
                       nrow(theImage()) * input$videoSize_x,
                       ncol(theImage()) * input$videoSize_x)
    }
  } else {
    Rvision::display(Rvision::zeros(480, 640), "trackR", 5, 480, 640)
  }
})

# Export video
shinyFileSave(input, "exportVideo_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$exportVideo_x, {
  path <- parseSavePath(volumes, input$exportVideo_x)
  theTrackVideoPath(path$datapath)
})

observeEvent(theTrackVideoPath(), {
  if (length(theTrackVideoPath()) > 0) {
    toggleAll("OFF")

    showNotification("Exporting video.", id = "exporting", duration = NULL)

    range_pos <- range(theTracks()[, frame])

    n <- diff(range_pos) + 1

    sc <- max(dim(theImage()) / 720)

    vw <- Rvision::videoWriter(theTrackVideoPath(),
                               fourcc = "avc1",
                               fps = theVideo()$fps(),
                               height = theVideo()$nrow(),
                               width = theVideo()$ncol())

    pb <- Progress$new()
    pb$set(message = "Processing: ", value = 0, detail = "0%")
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    for (i in 1:n) {
      pos <- i + range_pos[1] - 1
      tmp_rect <- theTracks()[ignore == FALSE & frame == pos, ]

      if (nrow(tmp_rect) > 0) {
        if (i == 1) {
          frame <- Rvision::readFrame(theVideo(), range_pos[1])
        } else {
          frame <- Rvision::readNext(theVideo())
        }

        overlay1 <- Rvision::cloneImage(frame)
        Rvision::drawRotatedRectangle(overlay1, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = 1.5 * sc)

        overlay2 <- Rvision::cloneImage(frame)
        Rvision::drawRotatedRectangle(overlay2, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = -1)

        tmp_tracks <- theTracks()[ignore == FALSE &
                                    frame >= (pos - 1 * theVideo()$fps()) &
                                    frame <= pos, ]

        tmp_tracks[, Rvision::drawPolyline(overlay2, cbind(x, y), FALSE,
                                           color = cbPalette[(track_fixed[1] %% 12) + 1],
                                           thickness = 3 * sc),
                   by = track_fixed]

        to_export <- Rvision::addWeighted(overlay1, overlay2, c(0.5, 0.5))
        Rvision::drawText(to_export, tmp_rect$track_fixed,
                          tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                          tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                          color = "white")

        Rvision::writeFrame(vw, to_export)

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
    }

    Rvision::release(vw)

    pb$close()
    removeNotification(id = "exporting")
    toggleAll("ON")
  }
})

