verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Data module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus"),
  htmlOutput("trackStatus"),

  tags$table(style = "width: 100%; margin-bottom: 10px;",
             tags$tr(
               shinyFilesButton("videoFile_x", "Select video file",
                                "Please select a video file", FALSE,
                                class = "halfWidth"),
               shinyFilesButton("trackFile_x", "Select track file",
                                "Please select a track file", FALSE,
                                class = "halfWidth")
             )
  )
)
