bsCollapsePanel(
  title = actionLink("toggleVideo", "Video file"),
  value = "videoPanel",
  shinyFilesButton("videoFile", "Select video file",
                   "Please select a video file", FALSE, class = "fullWidth"),
  htmlOutput("rangeSlider"),
  htmlOutput("displaySlider"),
  htmlOutput("qualitySlider"),
  htmlOutput("videoStatus")
)
