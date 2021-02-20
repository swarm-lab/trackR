verticalTabPanel(
  title = "5",
  box_height = "100%",
  p("Separation module", class = "module-title"),

  hr(),

  htmlOutput("blobStatus", container = p, class = "good"),

  actionButton("optimizeBlobs_x", "Automatically optimize object parameters", width = "100%"),

  # p(style = "padding-bottom: 10px;"),
  #
  # sliderInput("blobImages_x", "Number of frames for estimating blob parameters:",
  #             min = 1, max = 200, value = 100, width = "100%"),

  hr(class = "hr-text", `data-content` = "Or set them manually"),

  numericInput("blobWidth_x", "Maximum width (in pixels)", 0,
               min = 1, max = Inf, step = 1, width = "100%"),

  numericInput("blobHeight_x", "Maximum height (in pixels)", 0,
               min = 1, max = Inf, step = 1, width = "100%"),

  numericInput("blobArea_x", "Minimum area (in pixels)", 0,
               min = 1, max = Inf, step = 1, width = "100%"),

  numericInput("blobDensity_x", "Minimum density [0-1]", 0,
               min = 0, max = 1, step = 0.01, width = "100%"),

  hr(),

  htmlOutput("videoSlider3")
)
