disabled(
  verticalTabPanel(
    title = "3",
    box_height = "100%",
    p("Mask module", class = "module-title"),
    hr(),
    shinyFilesButton("mask_file_x", "Select existing mask",
      "Please select a mask file", FALSE,
      class = "fullWidth"
    ),
    hr(),
    tags$table(
      tags$tr(
        tags$td(actionButton("include_all_x", "Include all", width = "100%"),
          style = "width: 49%;"
        ),
        tags$td(),
        tags$td(actionButton("exclude_all_x", "Exclude all", width = "100%"),
          style = "width: 49%;"
        )
      ),
      tags$tr(),
      tags$tr(
        tags$td(
          actionButton("poly_button_x", "Add polygon ROI", width = "100%"),
          style = "width: 49%;"
        ),
        tags$td(),
        tags$td(
          actionButton("ell_button_x", "Add ellipse ROI", width = "100%"),
          style = "width: 49%;"
        )
      ),
      class = "settingsTable"
    ),
    p(),
    awesomeRadio(
      inputId = "inc_button_x", label = NULL,
      choices = c("Including", "Excluding"), selected = "Including",
      inline = TRUE, checkbox = TRUE, width = "100%"
    ),
    numericInput("roi_x", "ROI id", 1, 1, 255, 1, "100%"),
    hr(),
    shinySaveButton("save_mask_x", "Save mask file", "Save mask as...",
      filetype = list(picture = c("png", "jpg")),
      class = "fullWidth"
    ),
    p(style = "padding-bottom: 10px;")
  )
)
