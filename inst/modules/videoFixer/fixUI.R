verticalTabPanel(
  title = "2",
  id = "fixPanel",
  box_height = "100%",
  p("Fixing module", class = "module-title"),

  hr(),

  tags$label("Adjust lighting to match reference image",
             class = "control-label"),
  switchInput(inputId = "lightToggle_x", value = FALSE,
              onLabel = "YES", offLabel = "NO"),

  hr(),

  tags$label("Correct camera shift to match reference image",
             class = "control-label"),
  switchInput(inputId = "perspToggle_x", value = FALSE,
              onLabel = "YES", offLabel = "NO"),
  radioButtons(inputId = "perspSpeed_x", label = NULL, inline = TRUE,
               choices = c("Faster", "More precise")),

  hr(),

  tags$table(
    tags$tr(
      tags$td(
        sliderInput("refWidth_x", "Reference area width", width = "100%",
                    value = 1, min = 1, max = 1, step = 1),
        style = "width: 49%;"),
      tags$td(),
      tags$td(
        sliderInput("refHeight_x", "Reference area height", width = "100%",
                    value = 1, min = 1, max = 1, step = 1),
        style = "width: 49%;")
    ),

    tags$tr(),

    tags$tr(
      tags$td(
        sliderInput("refShiftLeft_x", "Reference shift left", width = "100%",
                    value = 0, min = 0, max = 0, step = 1),
        style = "width: 49%;"),
      tags$td(),
      tags$td(
        sliderInput("refShiftUp_x", "Reference shift up", width = "100%",
                    value = 0, min = 0, max = 0, step = 1),
        style = "width: 49%;")
    ),

    class = "settingsTable"
  ),

  hr(),

  shinySaveButton("exportVideo_x", "Export video", "Save video as...",
                  filetype = "mp4",
                  class = "fullWidth"),

  p()
)
