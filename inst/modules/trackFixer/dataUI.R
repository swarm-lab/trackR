verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Data module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus"),
  htmlOutput("trackStatus"),

  tags$table(
    tags$tr(
      tags$td(shinyFilesButton("videoFile_x", "Select video file",
                               "Please select a video file", FALSE,
                               class = "fullWidth"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(shinyFilesButton("trackFile_x", "Select track file",
                               "Please select a track file", FALSE,
                               class = "fullWidth"),
              style = "width: 49%;")
    ),

    class = "settingsTable"
  ),

  p()
)
