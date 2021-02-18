verticalTabPanel(
  title = "3",
  box_height = "100%",
  p("Mask module", class = "module-title"),

  hr(),

  shinyFilesButton("maskFile_x", "Select existing mask",
                   "Please select a mask file", FALSE, class = "fullWidth"),

  hr(),

  tags$table(style = "width: 100%; margin-bottom: 10px;",
             tags$tr(
               actionButton("includeAll_x", "Include all", width = "50%"),
               actionButton("excludeAll_x", "Exclude all", width = "50%")
             )
  ),

  tags$table(style = "width: 100%; margin-bottom: 10px;",
             tags$tr(
               actionButton("polyButton_x", "Add polygon ROI", width = "50%"),
               actionButton("ellButton_x", "Add ellipse ROI", width = "50%")
             )
  ),

  awesomeRadio(inputId = "incButton_x", label = NULL,
               choices=c("Including", "Excluding"), selected = "Including",
               inline = TRUE, checkbox = TRUE, width = "100%"),

  hr(),

  shinySaveButton("saveMask_x", "Save mask file", "Save mask as...",
                  filetype = list(picture = c("png", "jpg")),
                  class = "fullWidth"),

  p(style = "padding-bottom: 10px;")
)
