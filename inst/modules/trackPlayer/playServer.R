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


# Display slider
output$displaySlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})


# Range slider
output$rangeSlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                max = Rvision::nframes(theVideo()),
                value = c(1, Rvision::nframes(theVideo())), step = 1)
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


# Controls
play <- reactiveVal(FALSE)

observeEvent(input$videoPos_x, {
  if (input$videoPos_x - theVideo()$frame() == 1) {
    theImage(Rvision::readNext(theVideo()))
  } else {
    theImage(Rvision::readFrame(theVideo(), input$videoPos_x))
  }
})

observeEvent(input$videoSize_x, {
  if (!is.null(input$videoPos_x))
    theImage(Rvision::readFrame(theVideo(), input$videoPos_x))
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


# Title
title <- reactiveValues(text = "",
                        scale = 1,
                        thickness = 1,
                        color = "black",
                        vert_position = "Top",
                        hor_position = "Left",
                        margin_vert = -2,
                        margin_hor = 2)

observeEvent(input$title_x, {
  showModal(
    modalDialog(
      title = "Set title",
      easyClose = TRUE,

      textAreaInput("titleText", "Title text", value = title$text, width = "100%", rows = 3),

      hr(),

      tags$table(
        tags$tr(
          tags$td(numericInput("titleScale", "Font scale", value = title$scale,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("titleThick", "Font thickness", value = title$thickness,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(
            colourInput("titleColor", "Font color", title$color),
            colspan = "3",
            style = "width: 100%;"
          )
        ),

        class = "settingsTable"
      ),

      hr(),

      tags$table(
        tags$tr(
          tags$td(selectInput("titleVert", "Vertical position", c("Top", "Middle", "Bottom"),
                              selected = title$vert_position, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(selectInput("titleHor", "Horizontal position", c("Left", "Center", "Right"),
                              selected = title$hor_position, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(numericInput("titleMarginVert", "Vertical margin (% height)",
                               value = title$margin_vert,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("titleMarginHor", "Horizontal margin (% width)",
                               value = title$margin_hor,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        class = "settingsTable"
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okTitle", "Set")
      )
    )
  )
})

observe({
  if (!is.null(input$titleText) & !is.null(input$titleScale) &
      !is.null(input$titleThick) & !is.null(input$titleColor) &
      !is.null(input$titleVert) & !is.null(input$titleHor) &
      !is.null(input$titleMarginVert) & !is.null(input$titleMarginHor)) {
    isolate({
      title$text <- input$titleText
      title$scale <- input$titleScale
      title$thickness <- input$titleThick
      title$color <- input$titleColor
      title$vert_position <- input$titleVert
      title$hor_position <- input$titleHor
      title$margin_vert <- input$titleMarginVert
      title$margin_hor <- input$titleMarginHor
      refreshDisplay(refreshDisplay() + 1)
    })
  }
})

observeEvent(input$okTitle, {
  removeModal(session)
})


# Subtitle
subtitle <- reactiveValues(text = "",
                           scale = 0.75,
                           thickness = 1,
                           color = "black",
                           vert_position = "Top",
                           hor_position = "Left",
                           margin_vert = -10,
                           margin_hor = 2)

observeEvent(input$subtitle_x, {
  showModal(
    modalDialog(
      title = "Set subtitle",
      easyClose = TRUE,

      textAreaInput("subtitleText", "Subtitle text", value = subtitle$text, width = "100%", rows = 3),

      hr(),

      tags$table(
        tags$tr(
          tags$td(numericInput("subtitleScale", "Font scale", value = subtitle$scale,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("subtitleThick", "Font thickness", value = subtitle$thickness,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(
            colourInput("subtitleColor", "Font color", subtitle$color),
            colspan = "3",
            style = "width: 100%;"
          )
        ),

        class = "settingsTable"
      ),

      hr(),

      tags$table(
        tags$tr(
          tags$td(selectInput("subtitleVert", "Vertical position", c("Top", "Middle", "Bottom"),
                              selected = subtitle$vert_position, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(selectInput("subtitleHor", "Horizontal position", c("Left", "Center", "Right"),
                              selected = subtitle$hor_position, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(numericInput("subtitleMarginVert", "Vertical margin (% height)",
                               value = subtitle$margin_vert,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("subtitleMarginHor", "Horizontal margin (% width)",
                               value = subtitle$margin_hor,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        class = "settingsTable"
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okSubtitle", "Set")
      )
    )
  )
})

observe({
  if (!is.null(input$subtitleText) & !is.null(input$subtitleScale) &
      !is.null(input$subtitleThick) & !is.null(input$subtitleColor) &
      !is.null(input$subtitleVert) & !is.null(input$subtitleHor) &
      !is.null(input$subtitleMarginVert) & !is.null(input$subtitleMarginHor)) {
    isolate({
      subtitle$text <- input$subtitleText
      subtitle$scale <- input$subtitleScale
      subtitle$thickness <- input$subtitleThick
      subtitle$color <- input$subtitleColor
      subtitle$vert_position <- input$subtitleVert
      subtitle$hor_position <- input$subtitleHor
      subtitle$margin_vert <- input$subtitleMarginVert
      subtitle$margin_hor <- input$subtitleMarginHor
      refreshDisplay(refreshDisplay() + 1)
    })
  }
})

observeEvent(input$okSubtitle, {
  removeModal(session)
})


# Author list
authors <- reactiveValues(text = "",
                          scale = 0.75,
                          thickness = 1,
                          color = "black",
                          vert_position = "Bottom",
                          hor_position = "Left",
                          margin_vert = 2,
                          margin_hor = 2)

observeEvent(input$authors_x, {
  showModal(
    modalDialog(
      title = "Set author list",
      easyClose = TRUE,

      textAreaInput("authorsText", "Author list text", value = authors$text, width = "100%", rows = 3),

      hr(),

      tags$table(
        tags$tr(
          tags$td(numericInput("authorsScale", "Font scale", value = authors$scale,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("authorsThick", "Font thickness", value = authors$thickness,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(
            colourInput("authorsColor", "Font color", authors$color),
            colspan = "3",
            style = "width: 100%;"
          )
        ),

        class = "settingsTable"
      ),

      hr(),

      tags$table(
        tags$tr(
          tags$td(selectInput("authorsVert", "Vertical position", c("Top", "Middle", "Bottom"),
                              selected = authors$vert_position, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(selectInput("authorsHor", "Horizontal position", c("Left", "Center", "Right"),
                              selected = authors$hor_position, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(numericInput("authorsMarginVert", "Vertical margin (% height)",
                               value = authors$margin_vert,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("authorsMarginHor", "Horizontal margin (% width)",
                               value = authors$margin_hor,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        class = "settingsTable"
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okAuthors", "Set")
      )
    )
  )
})

observe({
  if (!is.null(input$authorsText) & !is.null(input$authorsScale) &
      !is.null(input$authorsThick) & !is.null(input$authorsColor) &
      !is.null(input$authorsVert) & !is.null(input$authorsHor) &
      !is.null(input$authorsMarginVert) & !is.null(input$authorsMarginHor)) {
    isolate({
      authors$text <- input$authorsText
      authors$scale <- input$authorsScale
      authors$thickness <- input$authorsThick
      authors$color <- input$authorsColor
      authors$vert_position <- input$authorsVert
      authors$hor_position <- input$authorsHor
      authors$margin_vert <- input$authorsMarginVert
      authors$margin_hor <- input$authorsMarginHor
      refreshDisplay(refreshDisplay() + 1)
    })
  }
})

observeEvent(input$okAuthors, {
  removeModal(session)
})


# Timestamp
timestamp <- reactiveValues(display = FALSE,
                            scale = 0.5,
                            thickness = 1,
                            color = "black",
                            vert_position = "Bottom",
                            hor_position = "Right",
                            margin_vert = 2,
                            margin_hor = -2)

observeEvent(input$timestamp_x, {
  showModal(
    modalDialog(
      title = "Set timestamp",
      easyClose = TRUE,

      tags$label("Display timestamp?",
                 class = "control-label"),
      switchInput(inputId = "timestampToggle_x",
                  value = timestamp$display,
                  onLabel = "YES", offLabel = "NO"),

      hr(),

      tags$table(
        tags$tr(
          tags$td(numericInput("timestampScale", "Font scale", value = timestamp$scale,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("timestampThick", "Font thickness", value = timestamp$thickness,
                               min = 0.1, max = 10, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(
            colourInput("timestampColor", "Font color", timestamp$color),
            colspan = "3",
            style = "width: 100%;"
          )
        ),

        class = "settingsTable"
      ),

      hr(),

      tags$table(
        tags$tr(
          tags$td(selectInput("timestampVert", "Vertical position", c("Top", "Middle", "Bottom"),
                              selected = timestamp$vert_position, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(selectInput("timestampHor", "Horizontal position", c("Left", "Center", "Right"),
                              selected = timestamp$hor_position, width = "100%"),
                  style = "width: 49%;")
        ),

        tags$tr(
          tags$td(numericInput("timestampMarginVert", "Vertical margin (% height)",
                               value = timestamp$margin_vert,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;"),
          tags$td(),
          tags$td(numericInput("timestampMarginHor", "Horizontal margin (% width)",
                               value = timestamp$margin_hor,
                               min = -100, max = 100, step = 0.1, width = "100%"),
                  style = "width: 49%;")
        ),

        class = "settingsTable"
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okTimestamp", "Set")
      )
    )
  )
})

observe({
  if (!is.null(input$timestampToggle_x) & !is.null(input$timestampScale) &
      !is.null(input$timestampThick) & !is.null(input$timestampColor) &
      !is.null(input$timestampVert) & !is.null(input$timestampHor) &
      !is.null(input$timestampMarginVert) & !is.null(input$timestampMarginHor)) {
    isolate({
      timestamp$display <- input$timestampToggle_x
      timestamp$scale <- input$timestampScale
      timestamp$thickness <- input$timestampThick
      timestamp$color <- input$timestampColor
      timestamp$vert_position <- input$timestampVert
      timestamp$hor_position <- input$timestampHor
      timestamp$margin_vert <- input$timestampMarginVert
      timestamp$margin_hor <- input$timestampMarginHor
      refreshDisplay(refreshDisplay() + 1)
    })
  }
})

observeEvent(input$okTimestamp, {
  removeModal(session)
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
    tmp_tracks <- theTracks()[ignore == FALSE &
                                frame >= (input$videoPos_x - 1 * theVideo()$fps()) &
                                frame <= input$videoPos_x, ]
    sc <- max(dim(theImage()) / 720)

    if (nrow(tmp_tracks) > 0) {
      overlay1 <- Rvision::cloneImage(theImage())
      overlay2 <- Rvision::cloneImage(theImage())

      if (nrow(tmp_rect) > 0) {
        Rvision::drawRotatedRectangle(overlay1, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = 1.5 * sc)
        Rvision::drawRotatedRectangle(overlay2, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = -1)
      }

      tmp_tracks[, Rvision::drawPolyline(overlay2, cbind(x, y), FALSE,
                                         color = cbPalette[(track_fixed[1] %% 12) + 1],
                                         thickness = 3 * sc),
                 by = track_fixed]

      to_display <- Rvision::addWeighted(overlay1, overlay2, c(0.5, 0.5))

      if (nrow(tmp_rect) > 0) {
        Rvision::drawText(to_display, tmp_rect$track_fixed,
                          tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                          tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                          color = "white")
      }
    } else {
      to_display <- Rvision::cloneImage(theImage())
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

    Rvision::display(to_display, "trackPlayer", 5,
                     nrow(to_display) * input$videoSize_x,
                     ncol(to_display) * input$videoSize_x)
  } else {
    Rvision::display(Rvision::zeros(480, 640), "trackPlayer", 5, 480, 640)
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

    range_pos <- input$rangePos_x # range(theTracks()[, frame])
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
      tmp_tracks <- theTracks()[ignore == FALSE &
                                  frame >= (pos - 1 * theVideo()$fps()) &
                                  frame <= pos, ]

      if (i == 1) {
        frame <- Rvision::readFrame(theVideo(), range_pos[1])
      } else {
        frame <- Rvision::readNext(theVideo())
      }

      if (nrow(tmp_tracks) > 0) {
        overlay1 <- Rvision::cloneImage(frame)
        overlay2 <- Rvision::cloneImage(frame)

        if (nrow(tmp_rect) > 0) {
          Rvision::drawRotatedRectangle(overlay1, tmp_rect$x, tmp_rect$y,
                                        tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                        color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                        thickness = 1.5 * sc)
          Rvision::drawRotatedRectangle(overlay2, tmp_rect$x, tmp_rect$y,
                                        tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                        color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                        thickness = -1)
        }

        tmp_tracks[, Rvision::drawPolyline(overlay2, cbind(x, y), FALSE,
                                           color = cbPalette[(track_fixed[1] %% 12) + 1],
                                           thickness = 3 * sc),
                   by = track_fixed]

        to_export <- Rvision::addWeighted(overlay1, overlay2, c(0.5, 0.5))

        if (nrow(tmp_rect) > 0) {
          Rvision::drawText(to_export, tmp_rect$track_fixed,
                            tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                            tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                            color = "white")
        }
      } else {
        to_export <- Rvision::cloneImage(frame)
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

