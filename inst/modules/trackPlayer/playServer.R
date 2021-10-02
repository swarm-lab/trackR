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


# Display slider
output$displaySlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})


# Range slider
output$rangeSlider <- renderUI({
  if (Rvision::isVideo(theVideo()) & is.data.frame(theTracks())) {
    sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                max = Rvision::nframes(theVideo()),
                value = range(theTracks()$frame), step = 1)
  }
})


# Video slider
rangeMem <- c(NA, NA)

output$videoSlider <- renderUI({
  if (Rvision::isVideo(theVideo()) & !is.null(input$rangePos_x)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      sliderInput("videoPos_x", "Frame", width = "100%", step = 1,
                  value = input$rangePos_x[2],
                  min = input$rangePos_x[1],
                  max = input$rangePos_x[2])
    } else {
      sliderInput("videoPos_x", "Frame", width = "100%", step = 1,
                  value = input$rangePos_x[1],
                  min = input$rangePos_x[1],
                  max = input$rangePos_x[2])
    }
  }
})


# Read video
refreshDisplay <- reactiveVal(0)
play <- reactiveVal(FALSE)
theImage <- NULL

observeEvent(input$videoPos_x, {
  if (isImage(theImage)) {
    if (input$videoPos_x - theVideo()$frame() == 1) {
      Rvision::readNext(theVideo(), theImage)
    } else {
      Rvision::readFrame(theVideo(), input$videoPos_x, theImage)
    }
  } else {
    theImage <<- Rvision::readFrame(theVideo(), input$videoPos_x)
  }

  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$videoSize_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$playPause_x, {
  if (!play() & Rvision::isVideo(theVideo())) {
    play(TRUE)
  } else {
    play(FALSE)
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
observeEvent(refreshDisplay(), {
  if (isImage(theImage)) {
    tmp_tracks <- theTracks()[ignore == FALSE &
                                frame >= (input$videoPos_x - 1 * theVideo()$fps()) &
                                frame <= input$videoPos_x, ]
    tmp_rect <- tmp_tracks[frame == input$videoPos_x, ]

    sc <- max(dim(theImage) / 720)

    to_display <- Rvision::cloneImage(theImage)

    if (nrow(tmp_tracks) > 0) {
      overlay <- Rvision::cloneImage(theImage)

      if (nrow(tmp_rect) > 0) {
        Rvision::drawRotatedRectangle(to_display, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = 1.5 * sc)
        Rvision::drawRotatedRectangle(overlay, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = -1)
      }

      tmp_tracks[, Rvision::drawPolyline(overlay, cbind(x, y), FALSE,
                                         color = cbPalette[(track_fixed[1] %% 12) + 1],
                                         thickness = 3 * sc),
                 by = track_fixed]

      Rvision::addWeighted(to_display, overlay, c(0.5, 0.5), target = to_display)

      if (nrow(tmp_rect) > 0) {
        Rvision::drawText(to_display, tmp_rect$track_fixed,
                          tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                          tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                          color = "white")
      }
    }

    if (nchar(title$text) > 0) {
      txt_size <- Rvision::getTextSize(title$text,
                                       font_face = "duplex",
                                       font_scale = title$scale * sc,
                                       thickness = title$thickness * sc)

      Rvision::drawText(to_display, title$text,
                        x = switch (title$hor_position,
                                    "Left" = 0 + (title$margin_hor / 100) * ncol(to_display),
                                    "Center" = ((ncol(to_display) - txt_size[2]) / 2) +
                                      (title$margin_hor / 100) * ncol(to_display),
                                    "Right" = (ncol(to_display) - txt_size[2]) +
                                      (title$margin_hor / 100) * ncol(to_display)
                        ),
                        y = switch (title$vert_position,
                                    "Bottom" = 0 + (title$margin_vert / 100) * nrow(to_display),
                                    "Middle" = (nrow(to_display) - txt_size[1]) / 2 +
                                      (title$margin_vert / 100) * nrow(to_display),
                                    "Top" = (nrow(to_display) - txt_size[1]) +
                                      (title$margin_vert / 100) * nrow(to_display)
                        ),
                        font_scale = title$scale * sc,
                        thickness = title$thickness * sc,
                        color = title$color,
                        font_face = "duplex")
    }

    if (nchar(subtitle$text) > 0) {
      txt_size <- Rvision::getTextSize(subtitle$text,
                                       font_face = "duplex",
                                       font_scale = subtitle$scale * sc,
                                       thickness = subtitle$thickness * sc)

      Rvision::drawText(to_display, subtitle$text,
                        x = switch (subtitle$hor_position,
                                    "Left" = 0 + (subtitle$margin_hor / 100) * ncol(to_display),
                                    "Center" = ((ncol(to_display) - txt_size[2]) / 2) +
                                      (subtitle$margin_hor / 100) * ncol(to_display),
                                    "Right" = (ncol(to_display) - txt_size[2]) +
                                      (subtitle$margin_hor / 100) * ncol(to_display)
                        ),
                        y = switch (subtitle$vert_position,
                                    "Bottom" = 0 + (subtitle$margin_vert / 100) * nrow(to_display),
                                    "Middle" = (nrow(to_display) - txt_size[1]) / 2 +
                                      (subtitle$margin_vert / 100) * nrow(to_display),
                                    "Top" = (nrow(to_display) - txt_size[1]) +
                                      (subtitle$margin_vert / 100) * nrow(to_display)
                        ),
                        font_scale = subtitle$scale * sc,
                        thickness = subtitle$thickness * sc,
                        color = subtitle$color,
                        font_face = "duplex")
    }

    if (nchar(authors$text) > 0) {
      txt_size <- Rvision::getTextSize(authors$text,
                                       font_face = "duplex",
                                       font_scale = authors$scale * sc,
                                       thickness = authors$thickness * sc)

      Rvision::drawText(to_display, authors$text,
                        x = switch (authors$hor_position,
                                    "Left" = 0 + (authors$margin_hor / 100) * ncol(to_display),
                                    "Center" = ((ncol(to_display) - txt_size[2]) / 2) +
                                      (authors$margin_hor / 100) * ncol(to_display),
                                    "Right" = (ncol(to_display) - txt_size[2]) +
                                      (authors$margin_hor / 100) * ncol(to_display)
                        ),
                        y = switch (authors$vert_position,
                                    "Bottom" = 0 + (authors$margin_vert / 100) * nrow(to_display),
                                    "Middle" = (nrow(to_display) - txt_size[1]) / 2 +
                                      (authors$margin_vert / 100) * nrow(to_display),
                                    "Top" = (nrow(to_display) - txt_size[1]) +
                                      (authors$margin_vert / 100) * nrow(to_display)
                        ),
                        font_scale = authors$scale * sc,
                        thickness = authors$thickness * sc,
                        color = authors$color,
                        font_face = "duplex")
    }

    if (timestamp$display) {
      txt <- hmsf(input$videoPos_x, theVideo()$fps())
      txt_size <- Rvision::getTextSize(txt,
                                       font_face = "duplex",
                                       font_scale = timestamp$scale * sc,
                                       thickness = timestamp$thickness * sc)

      Rvision::drawText(to_display, txt,
                        x = switch (timestamp$hor_position,
                                    "Left" = 0 + (timestamp$margin_hor / 100) * ncol(to_display),
                                    "Center" = ((ncol(to_display) - txt_size[2]) / 2) +
                                      (timestamp$margin_hor / 100) * ncol(to_display),
                                    "Right" = (ncol(to_display) - txt_size[2]) +
                                      (timestamp$margin_hor / 100) * ncol(to_display)
                        ),
                        y = switch (timestamp$vert_position,
                                    "Bottom" = 0 + (timestamp$margin_vert / 100) * nrow(to_display),
                                    "Middle" = (nrow(to_display) - txt_size[1]) / 2 +
                                      (timestamp$margin_vert / 100) * nrow(to_display),
                                    "Top" = (nrow(to_display) - txt_size[1]) +
                                      (timestamp$margin_vert / 100) * nrow(to_display)
                        ),
                        font_scale = timestamp$scale * sc,
                        thickness = timestamp$thickness * sc,
                        color = timestamp$color,
                        font_face = "duplex")
    }

    Rvision::display(to_display, "trackPlayer", 1,
                     nrow(to_display) * input$videoSize_x,
                     ncol(to_display) * input$videoSize_x)
  } else {
    Rvision::display(Rvision::zeros(480, 640), "trackPlayer", 1, 480, 640)
  }
})

# Export video
theTrackVideoPath <- reactiveVal()

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

    range_pos <- input$rangePos_x # range(theTracks()[, frame])
    n <- diff(range_pos) + 1
    sc <- max(dim(theImage) / 720)

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
      tmp_tracks <- theTracks()[ignore == FALSE &
                                  frame >= (pos - 1 * theVideo()$fps()) &
                                  frame <= pos, ]
      tmp_rect <- tmp_tracks[frame == pos, ]

      if (i == 1) {
        to_export <- Rvision::readFrame(theVideo(), range_pos[1])
      } else {
        Rvision::readNext(theVideo(), to_export)
      }

      if (nrow(tmp_tracks) > 0) {
        overlay <- Rvision::cloneImage(to_export)

        if (nrow(tmp_rect) > 0) {
          Rvision::drawRotatedRectangle(to_export, tmp_rect$x, tmp_rect$y,
                                        tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                        color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                        thickness = 1.5 * sc)
          Rvision::drawRotatedRectangle(overlay, tmp_rect$x, tmp_rect$y,
                                        tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                        color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                        thickness = -1)
        }

        tmp_tracks[, Rvision::drawPolyline(overlay, cbind(x, y), FALSE,
                                           color = cbPalette[(track_fixed[1] %% 12) + 1],
                                           thickness = 3 * sc),
                   by = track_fixed]

        Rvision::addWeighted(to_export, overlay, c(0.5, 0.5), target = to_export)

        if (nrow(tmp_rect) > 0) {
          Rvision::drawText(to_export, tmp_rect$track_fixed,
                            tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                            tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                            color = "white")
        }
      }

      if (nchar(title$text) > 0) {
        txt_size <- Rvision::getTextSize(title$text,
                                         font_face = "duplex",
                                         font_scale = title$scale * sc,
                                         thickness = title$thickness * sc)

        Rvision::drawText(to_export, title$text,
                          x = switch (title$hor_position,
                                      "Left" = 0 + (title$margin_hor / 100) * ncol(to_export),
                                      "Center" = ((ncol(to_export) - txt_size[2]) / 2) +
                                        (title$margin_hor / 100) * ncol(to_export),
                                      "Right" = (ncol(to_export) - txt_size[2]) +
                                        (title$margin_hor / 100) * ncol(to_export)
                          ),
                          y = switch (title$vert_position,
                                      "Bottom" = 0 + (title$margin_vert / 100) * nrow(to_export),
                                      "Middle" = (nrow(to_export) - txt_size[1]) / 2 +
                                        (title$margin_vert / 100) * nrow(to_export),
                                      "Top" = (nrow(to_export) - txt_size[1]) +
                                        (title$margin_vert / 100) * nrow(to_export)
                          ),
                          font_scale = title$scale * sc,
                          thickness = title$thickness * sc,
                          color = title$color,
                          font_face = "duplex")
      }

      if (nchar(subtitle$text) > 0) {
        txt_size <- Rvision::getTextSize(subtitle$text,
                                         font_face = "duplex",
                                         font_scale = subtitle$scale * sc,
                                         thickness = subtitle$thickness * sc)

        Rvision::drawText(to_export, subtitle$text,
                          x = switch (subtitle$hor_position,
                                      "Left" = 0 + (subtitle$margin_hor / 100) * ncol(to_export),
                                      "Center" = ((ncol(to_export) - txt_size[2]) / 2) +
                                        (subtitle$margin_hor / 100) * ncol(to_export),
                                      "Right" = (ncol(to_export) - txt_size[2]) +
                                        (subtitle$margin_hor / 100) * ncol(to_export)
                          ),
                          y = switch (subtitle$vert_position,
                                      "Bottom" = 0 + (subtitle$margin_vert / 100) * nrow(to_export),
                                      "Middle" = (nrow(to_export) - txt_size[1]) / 2 +
                                        (subtitle$margin_vert / 100) * nrow(to_export),
                                      "Top" = (nrow(to_export) - txt_size[1]) +
                                        (subtitle$margin_vert / 100) * nrow(to_export)
                          ),
                          font_scale = subtitle$scale * sc,
                          thickness = subtitle$thickness * sc,
                          color = subtitle$color,
                          font_face = "duplex")
      }

      if (nchar(authors$text) > 0) {
        txt_size <- Rvision::getTextSize(authors$text,
                                         font_face = "duplex",
                                         font_scale = authors$scale * sc,
                                         thickness = authors$thickness * sc)

        Rvision::drawText(to_export, authors$text,
                          x = switch (authors$hor_position,
                                      "Left" = 0 + (authors$margin_hor / 100) * ncol(to_export),
                                      "Center" = ((ncol(to_export) - txt_size[2]) / 2) +
                                        (authors$margin_hor / 100) * ncol(to_export),
                                      "Right" = (ncol(to_export) - txt_size[2]) +
                                        (authors$margin_hor / 100) * ncol(to_export)
                          ),
                          y = switch (authors$vert_position,
                                      "Bottom" = 0 + (authors$margin_vert / 100) * nrow(to_export),
                                      "Middle" = (nrow(to_export) - txt_size[1]) / 2 +
                                        (authors$margin_vert / 100) * nrow(to_export),
                                      "Top" = (nrow(to_export) - txt_size[1]) +
                                        (authors$margin_vert / 100) * nrow(to_export)
                          ),
                          font_scale = authors$scale * sc,
                          thickness = authors$thickness * sc,
                          color = authors$color,
                          font_face = "duplex")
      }

      if (timestamp$display) {
        txt <- hmsf(i, theVideo()$fps())
        txt_size <- Rvision::getTextSize(txt,
                                         font_face = "duplex",
                                         font_scale = timestamp$scale * sc,
                                         thickness = timestamp$thickness * sc)

        Rvision::drawText(to_export, txt,
                          x = switch (timestamp$hor_position,
                                      "Left" = 0 + (timestamp$margin_hor / 100) * ncol(to_export),
                                      "Center" = ((ncol(to_export) - txt_size[2]) / 2) +
                                        (timestamp$margin_hor / 100) * ncol(to_export),
                                      "Right" = (ncol(to_export) - txt_size[2]) +
                                        (timestamp$margin_hor / 100) * ncol(to_export)
                          ),
                          y = switch (timestamp$vert_position,
                                      "Bottom" = 0 + (timestamp$margin_vert / 100) * nrow(to_export),
                                      "Middle" = (nrow(to_export) - txt_size[1]) / 2 +
                                        (timestamp$margin_vert / 100) * nrow(to_export),
                                      "Top" = (nrow(to_export) - txt_size[1]) +
                                        (timestamp$margin_vert / 100) * nrow(to_export)
                          ),
                          font_scale = timestamp$scale * sc,
                          thickness = timestamp$thickness * sc,
                          color = timestamp$color,
                          font_face = "duplex")
      }


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

    Rvision::release(vw)

    pb$close()
    removeNotification(id = "exporting")
    toggleAll("ON")
  }
})

