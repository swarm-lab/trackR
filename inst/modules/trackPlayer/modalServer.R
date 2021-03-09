##########
# Title
##########
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

observeEvent(input$titleText, {
  if (!is.null(input$titleText)) {
    title$text <- input$titleText
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleScale, {
  if (!is.null(input$titleScale)) {
    title$scale <- input$titleScale
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleThick, {
  if (!is.null(input$titleThick)) {
    title$thickness <- input$titleThick
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleColor, {
  if (!is.null(input$titleColor)) {
    title$color <- input$titleColor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleVert, {
  if (!is.null(input$titleVert)) {
    title$vert_position <- input$titleVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleHor, {
  if (!is.null(input$titleHor)) {
    title$hor_position <- input$titleHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleMarginVert, {
  if (!is.null(input$titleMarginVert)) {
    title$margin_vert <- input$titleMarginVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$titleMarginHor, {
  if (!is.null(input$titleMarginHor)) {
    title$margin_hor <- input$titleMarginHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$okTitle, {
  removeModal(session)
})


##########
# Subtitle
##########
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

observeEvent(input$subtitleText, {
  if (!is.null(input$subtitleText)) {
    subtitle$text <- input$subtitleText
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleScale, {
  if (!is.null(input$subtitleScale)) {
    subtitle$scale <- input$subtitleScale
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleThick, {
  if (!is.null(input$subtitleThick)) {
    subtitle$thickness <- input$subtitleThick
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleColor, {
  if (!is.null(input$subtitleColor)) {
    subtitle$color <- input$subtitleColor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleVert, {
  if (!is.null(input$subtitleVert)) {
    subtitle$vert_position <- input$subtitleVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleHor, {
  if (!is.null(input$subtitleHor)) {
    subtitle$hor_position <- input$subtitleHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleMarginVert, {
  if (!is.null(input$subtitleMarginVert)) {
    subtitle$margin_vert <- input$subtitleMarginVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$subtitleMarginHor, {
  if (!is.null(input$subtitleMarginHor)) {
    subtitle$margin_hor <- input$subtitleMarginHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$okSubtitle, {
  removeModal(session)
})


##########
# Authors
##########
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

observeEvent(input$authorsText, {
  if (!is.null(input$authorsText)) {
    authors$text <- input$authorsText
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsScale, {
  if (!is.null(input$authorsScale)) {
    authors$scale <- input$authorsScale
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsThick, {
  if (!is.null(input$authorsThick)) {
    authors$thickness <- input$authorsThick
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsColor, {
  if (!is.null(input$authorsColor)) {
    authors$color <- input$authorsColor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsVert, {
  if (!is.null(input$authorsVert)) {
    authors$vert_position <- input$authorsVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsHor, {
  if (!is.null(input$authorsHor)) {
    authors$hor_position <- input$authorsHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsMarginVert, {
  if (!is.null(input$authorsMarginVert)) {
    authors$margin_vert <- input$authorsMarginVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$authorsMarginHor, {
  if (!is.null(input$authorsMarginHor)) {
    authors$margin_hor <- input$authorsMarginHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$okAuthors, {
  removeModal(session)
})


##########
# Timestamp
##########
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


observeEvent(input$timestampToggle_x, {
  if (!is.null(input$timestampToggle_x)) {
    timestamp$display <- input$timestampToggle_x
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampScale, {
  if (!is.null(input$timestampScale)) {
    timestamp$scale <- input$timestampScale
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampThick, {
  if (!is.null(input$timestampThick)) {
    timestamp$thickness <- input$timestampThick
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampColor, {
  if (!is.null(input$timestampColor)) {
    timestamp$color <- input$timestampColor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampVert, {
  if (!is.null(input$timestampVert)) {
    timestamp$vert_position <- input$timestampVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampHor, {
  if (!is.null(input$timestampHor)) {
    timestamp$hor_position <- input$timestampHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampMarginVert, {
  if (!is.null(input$timestampMarginVert)) {
    timestamp$margin_vert <- input$timestampMarginVert
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$timestampMarginHor, {
  if (!is.null(input$timestampMarginHor)) {
    timestamp$margin_hor <- input$timestampMarginHor
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$okTimestamp, {
  removeModal(session)
})
