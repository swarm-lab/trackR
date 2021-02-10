verticalTabPanel(
  title = "2",
  id = "fixPanel",
  box_height = "100%",
  p("Fixing module", class = "module-title"),

  hr(),

  tags$label("Adjust lighting to match reference image",
             class = "control-label"),
  switchInput(inputId = "lightToggle_x", value = TRUE,
              onLabel = "YES", offLabel = "NO"),

  hr(),

  tags$label("Correct camera shift to match reference image",
             class = "control-label"),
  switchInput(inputId = "perspToggle_x", value = TRUE,
              onLabel = "YES", offLabel = "NO"),

  hr(),

  shinySaveButton("exportVideo_x", "Export video", "Save video as...",
                  filetype = "mp4",
                  class = "fullWidth"),

  p()
)
