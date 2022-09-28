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

  tags$table(
    tags$tr(
      tags$td(actionButton("optimizeThresholds_x", "Automatically select thresholds", width = "100%"),
              style = "width: 49%;"),
      tags$td(HTML("&nbsp;with&nbsp;")),
      tags$td(selectInput("thresholdMethod_x", "", c(
        "Otsu's method" = "Otsu",
        "Renyi's entropy" = "RenyiEntropy",
        "ImageJ's method" = "ImageJ",
        "Huang's method" = "Huang",
        "Huang-Schindelin's method" = "Huang2",
        "Intermodes method" = "Intermodes",
        "Minimum method" = "Minimum",
        "Ridler-Calvar's isodata" = "IsoData",
        "Li's method" = "Li",
        "Kapur-Sahoo-Wong's method" = "MaxEntropy",
        "Mean method" = "Mean",
        "Kittler-Illingworth’s method" = "MinErrorI",
        "Tsai’s method" = "Moments",
        "Percentile method" = "Percentile",
        "Shanbhag's method" = "Shanbhag",
        "Triangle method" = "Triangle",
        "Yen's method" = "Yen"
      ), width = "100%"),
              style = "width: 49%;")
    )
  ),

  # actionButton("optimizeThresholds_x", "Automatically select thresholds", width = "100%"),

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
