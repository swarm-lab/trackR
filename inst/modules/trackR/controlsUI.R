verticalTabPanel(
  title = "X",
  box_height = "100%",

  tags$table(
    tags$tr(
      tags$td(shinySaveButton("saveSettings_x", "Save settings", "Save settings as...",
                              filetype = list(R = c("Rds", "rds")),
                              class = "fullWidth"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(shinyFilesButton("loadSettings_x", "Load settings",
                               "Please select a settings file", FALSE,
                               class = "fullWidth"),
              style = "width: 49%;")
    ),

    tags$tr(),

    tags$tr(
      tags$td(
        colspan = "3",
        actionButton("reset_x", "Reset trackR", width = "100%")
      )
    ),

    class = "settingsTable"
  ),

  p()
)
