bsCollapse(
  id = "controls",
  open = "controlPanel",
  bsCollapsePanel(
    title = NULL,
    value = "controlPanel",
    htmlOutput("videoSlider"),
    column(width = 6, shinyFilesButton("loadSettings", "Load settings",
                                       "Please select a settings file", FALSE, class = "fullWidth")),
    column(width = 6, shinySaveButton("saveSettings", "Save settings", "Save settings as...",
                                      filetype = list(R = c("Rds", "rds")),
                                      class = "fullWidth"))
  )
)
