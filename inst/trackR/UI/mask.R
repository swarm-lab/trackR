bsCollapsePanel(
  title = actionLink("toggleMask", "Mask"),
  value = "maskPanel",
  shinyFilesButton("maskFile", "Select mask file",
                   "Please select a mask file", FALSE, class = "fullWidth"),
  htmlOutput("maskStatus")
)
