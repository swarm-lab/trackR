disabled(
  verticalTabPanel(
    title = "3",
    box_height = "100%",
    p("Mask module", class = "module-title"),

    hr(),

    shinyFilesButton("maskFile_x", "Select existing mask",
                     "Please select a mask file", FALSE, class = "fullWidth"),

    hr(),

    tags$table(
      tags$tr(
        tags$td(actionButton("includeAll_x", "Include all", width = "100%"),
                style = "width: 49%;"),
        tags$td(),
        tags$td(actionButton("excludeAll_x", "Exclude all", width = "100%"),
                style = "width: 49%;")
      ),

      tags$tr(),

      tags$tr(
        tags$td(actionButton("polyButton_x", "Add polygon ROI", width = "100%"),
                style = "width: 49%;"),
        tags$td(),
        tags$td(actionButton("ellButton_x", "Add ellipse ROI", width = "100%"),
                style = "width: 49%;")
      ),

      class = "settingsTable"
    ),

    p(),

    awesomeRadio(inputId = "incButton_x", label = NULL,
                 choices=c("Including", "Excluding"), selected = "Including",
                 inline = TRUE, checkbox = TRUE, width = "100%"),

    numericInput("roi_x", "ROI id", 1, 1, 255, 1, "100%"),

    hr(),

    shinySaveButton("saveMask_x", "Save mask file", "Save mask as...",
                    filetype = list(picture = c("png", "jpg")),
                    class = "fullWidth"),

    p(style = "padding-bottom: 10px;")
  )
)
