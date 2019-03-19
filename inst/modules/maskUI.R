bsCollapsePanel(
  title = actionLink("toggleMask", "Mask"),
  value = "maskPanel",
  htmlOutput("maskStatus"),
  shinyFilesButton("maskFile", "Select mask file",
                   "Please select a mask file", FALSE, class = "fullWidth")
)
