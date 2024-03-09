disabled(
  verticalTabPanel(
    title = "6",
    box_height = "100%",
    p("Tracking module", class = "module-title"),
    hr(),
    tags$table(
      tags$tr(
        tags$td(actionButton("scale_x", "Set scale", width = "100%"),
          style = "width: 49%;"
        ),
        tags$td(),
        tags$td(actionButton("origin_x", "Set origin", width = "100%"),
          style = "width: 49%;"
        )
      ),
      class = "settingsTable"
    ),
    p(),
    htmlOutput("scale_status", container = p),
    hr(),
    sliderInput("look_back_x", "Look back (frames):",
      min = 1, max = 150,
      value = 5, width = "100%"
    ),
    sliderInput("maxDist_x", "Maximum distance (pixels):",
      min = 1, max = 200,
      value = 30, width = "100%"
    ),
    hr(),
    awesomeRadio(
      inputId = "show_tracks_x",
      label = "Display tracks during tracking (slower)",
      choices = c("Yes", "No"), selected = "No",
      inline = TRUE, checkbox = TRUE, width = "100%"
    ),
    hr(),
    awesomeRadio(
      inputId = "do_not_track_x", label = "Save positions only (do not track)",
      choices = c("Yes", "No"), selected = "No",
      inline = TRUE, checkbox = TRUE, width = "100%"
    ),
    hr(),
    shinySaveButton("compute_tracks_x", "Start tracking", "Save tracks as...",
      filetype = "csv",
      class = "fullWidth"
    ),
    p()
  )
)
