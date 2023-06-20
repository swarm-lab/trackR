verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Video module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus"),

  tags$div(
    class = "panel panel-default",
    shinyFilesButton("videoFile_x", "Add video(s)", multiple = TRUE,
                     "Please select video file(s)", FALSE, class = "fullWidth")
  ),

  htmlOutput("videoList"),

  hr(),

  htmlOutput("rangeSlider"),

  # htmlOutput("displaySlider"),

  htmlOutput("qualitySlider"),

  htmlOutput("videoSlider")
)
