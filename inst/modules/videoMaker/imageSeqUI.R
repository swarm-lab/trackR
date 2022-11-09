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

  hr(),

  tags$table(
    tags$tr(
      tags$td(htmlOutput("fpsLabel"), style = "width: 5%;"),
      tags$td(htmlOutput("fpsSelector"),
              style = "width: 44%;"),
      tags$td(),
      tags$td(htmlOutput("exportButton"),
              style = "width: 49%;")
    ),

    class = "settingsTable"
  ),

  p()
)
