disabled(
  verticalTabPanel(
    title = "5",
    box_height = "100%",
    p("Separation module", class = "module-title"),

    hr(),

    actionButton("optimize_blobs_x", "Automatically select object parameters", width = "100%"),

    hr(class = "hr-text", `data-content` = "Or set them manually"),

    numericInput("blob_width_x", "Maximum width (in pixels)", 0,
                 min = 1, max = Inf, step = 1, width = "100%"),

    numericInput("blob_height_x", "Maximum height (in pixels)", 0,
                 min = 1, max = Inf, step = 1, width = "100%"),

    numericInput("blob_area_x", "Minimum area (in pixels)", 0,
                 min = 1, max = Inf, step = 1, width = "100%"),

    numericInput("blob_density_x", "Minimum density [0-1]", 0,
                 min = 0, max = 1, step = 0.01, width = "100%"),

    hr(),

    htmlOutput("video_slider3")
  )
)
