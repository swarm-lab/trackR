disabled(
  verticalTabPanel(
    title = "6",
    box_height = "100%",
    p("Tracking module", class = "module-title"),

    hr(),

    htmlOutput("trackingStatus", container = p, class = "good"),

    tags$table(
      tags$tr(
        tags$td(actionButton("scale_x", "Set scale", width = "100%"),
                style = "width: 49%;"),
        tags$td(),
        tags$td(actionButton("origin_x", "Set origin", width = "100%"),
                style = "width: 49%;")
      ),

      class = "settingsTable"
    ),

    p(),

    htmlOutput("scaleStatus", container = p),

    hr(),

    # sliderInput("speedup_x", "Speed-up:", min = 1, max = 5, step = 1, value = 4,
    #             width = "100%"),

    sliderInput("lookBack_x", "Look back (frames):", min = 1, max = 150,
                value = 5, width = "100%"),

    sliderInput("maxDist_x", "Maximum distance (pixels):", min = 1, max = 200,
                value = 30, width = "100%"),

    hr(),

    awesomeRadio(inputId = "showTracks_x", label = "Display tracks during tracking (slower)",
                 choices = c("Yes", "No"), selected = "No",
                 inline = TRUE, checkbox = TRUE, width = "100%"),

    hr(),

    awesomeRadio(inputId = "doNotTrack_x", label = "Save positions only (do not track)",
                 choices = c("Yes", "No"), selected = "No",
                 inline = TRUE, checkbox = TRUE, width = "100%"),

    hr(),

    shinySaveButton("computeTracks_x", "Start tracking", "Save tracks as...",
                    filetype = "csv",
                    class = "fullWidth"),

    p()
  )
)
