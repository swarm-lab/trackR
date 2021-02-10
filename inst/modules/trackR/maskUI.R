verticalTabPanel(
  title = "3",
  box_height = "100%",
  p("Mask module", class = "module-title"),

  hr(),

  shinyFilesButton("maskFile_x", "Select existing mask",
                   "Please select a mask file", FALSE, class = "fullWidth"),

  hr(),

  actionButton("resetMask_x", "Reset mask", width = "100%"),

  hr(),

  actionButton("polyButton_x", "Add polygon ROI", width = "100%"),

  p(style = "padding-bottom: 10px;"),

  actionButton("ellButton_x", "Add ellipse ROI", width = "100%"),

  p(style = "padding-bottom: 10px;"),

  awesomeRadio(inputId = "incButton_x", label = "ROI type",
               choices=c("Including", "Excluding", "Inverse"), selected = "Including",
               inline = TRUE, checkbox = TRUE, width = "100%"),

  hr(),

  shinySaveButton("saveMask_x", "Save mask file", "Save mask as...",
                  filetype = list(picture = c("png", "jpg")),
                  class = "fullWidth"),

  p(style = "padding-bottom: 10px;")
)
