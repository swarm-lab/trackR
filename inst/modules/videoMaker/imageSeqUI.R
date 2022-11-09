verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Loading module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus"),

  shinyDirButton("sequenceFiles_x", "Select image sequence",
                 "Please select a folder containing an image sequence",
                 class = "fullWidth"),

  hr(),

  htmlOutput("displaySlider"),

  htmlOutput("rangeSlider"),

  htmlOutput("seqSlider"),

  tags$table(
    tags$tr(
      tags$td(tags$div(htmlOutput("compressionSelector"), style = "width: 150px; margin: auto;"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(htmlOutput("fpsLabel"), style = "width: 7%;"),
      tags$td(htmlOutput("fpsSelector"),
              style = "width: 42%;"),
    ),

    class = "settingsTable"
  ),

  htmlOutput("exportButton"),

  p()
)
