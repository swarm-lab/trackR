verticalTabPanel(
  title = "4",
  box_height = "100%",
  p("Segmentation module", class = "module-title"),

  hr(),

  htmlOutput("segmentationStatus", container = p, class = "good"),

  awesomeRadio(inputId = "darkButton_x", label = "Are objects darker or lighter than background?",
               choices = c("Darker", "Lighter"), selected = "Darker",
               inline = TRUE, checkbox = TRUE, width = "100%"),

  hr(class = "hr-text", `data-content` = "And either..."),

  actionButton("optimizeThresholds_x", "Automatically select thresholds", width = "100%"),

  hr(class = "hr-text", `data-content` = "Or set them manually"),

  sliderInput("redThreshold_x", "RGB thresholds", width = "100%", min = 1, max = 255,
              value = 30, step = 1),

  sliderInput("greenThreshold_x", NULL, width = "100%", min = 1, max = 255,
              value = 30, step = 1),

  sliderInput("blueThreshold_x", NULL, width = "100%", min = 1, max = 255,
              value = 30, step = 1),

  hr(),

  htmlOutput("videoSlider2")
)
