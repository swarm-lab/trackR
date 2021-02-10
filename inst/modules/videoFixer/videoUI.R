verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Video module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus"),

  shinyFilesButton("videoFile_x", "Select video file",
                   "Please select a video file", FALSE,
                   class = "fullWidth"),

  hr(),

  htmlOutput("displaySlider"),

  htmlOutput("rangeSlider"),

  htmlOutput("videoSlider")
)
