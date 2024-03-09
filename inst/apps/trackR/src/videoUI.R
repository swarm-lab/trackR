verticalTabPanel(
  title = "1",
  box_height = "100%",
  p("Video module", class = "module-title"),

  hr(),

  htmlOutput("video_status"),

  tags$div(
    class = "panel panel-default",
    shinyFilesButton("video_file_x", "Add video(s)", multiple = TRUE,
                     "Please select video file(s)", FALSE, class = "fullWidth")
  ),

  htmlOutput("video_list"),

  hr(),

  htmlOutput("range_slider"),

  htmlOutput("video_slider")
)
