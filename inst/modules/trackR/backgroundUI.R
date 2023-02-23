disabled(
  verticalTabPanel(
    title = "2",
    box_height = "100%",
    p("Background module", class = "module-title"),

    hr(),

    htmlOutput("backgroundStatus", container = p, class = "good"),

    shinyFilesButton("backgroundFile_x", "Select existing background",
                     "Please select a background image", FALSE, class = "fullWidth"),

    hr(class = "hr-text", `data-content` = "Or"),

    actionButton("computeBackground_x", "Automatically estimate background", width = "100%"),

    p(style = "padding-bottom: 10px;"),

    selectInput("backgroundType_x", "Background type:",
                choices = c("Median" = "median", "Mean" = "mean", "Minimum" = "min", "Maximum" = "max"),
                width = "100%"),

    sliderInput("backroundImages_x", "Number of frames for estimating background:", min = 1, max = 200,
                value = 25, width = "100%"),

    hr(),

    actionButton("ghostButton_x", "Select ghost for removal", width = "100%"),

    hr(),

    shinySaveButton("saveBackground_x", "Save background file", "Save background as...",
                    filetype = list(picture = c("png", "jpg")),
                    class = "fullWidth"),

    p(style = "padding-bottom: 10px;")
  )
)
