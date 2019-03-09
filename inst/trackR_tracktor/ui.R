shinyUI(fluidPage(

  titlePanel("trackR"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  useShinyjs(),

  fluidRow(
    column(
      12,

      bsCollapse(

        id = "bsCollapse",
        multiple = TRUE,
        open = "videoPanel",

        source("UI/video.R", local = TRUE)$value,

        # source("UI/background.R", local = TRUE)$value,

        source("UI/mask.R", local = TRUE)$value,

        source("UI/blob.R", local = TRUE)$value,

        source("UI/tracking.R", local = TRUE)$value

      ),

      bsCollapse(
        open = "controlPanel",
        bsCollapsePanel(
          title = NULL,
          value = "controlPanel",
          htmlOutput("videoSlider"),
          column(width = 6, shinyFilesButton("loadSettings", "Load settings",
                           "Please select a settings file", FALSE, class = "fullWidth")),
          column(width = 6, shinySaveButton("saveSettings", "Save settings", "Save settings as...",
                          filetype = list(R = ".Rda"),
                          class = "fullWidth"))
        )
      )
    )
  )

))
