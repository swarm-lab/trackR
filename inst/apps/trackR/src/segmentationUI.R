disabled(
  verticalTabPanel(
    title = "4",
    box_height = "100%",
    p("Segmentation module", class = "module-title"),
    hr(),
    htmlOutput("segmentation_status", container = p, class = "good"),
    awesomeRadio(
      inputId = "dark_button_x",
      label = "Are the objects darker or lighter than the background?",
      choices = c("Darker", "Lighter", "A bit of both"), selected = "Darker",
      inline = TRUE, checkbox = TRUE, width = "100%"
    ),
    hr(class = "hr-text", `data-content` = "And either..."),
    tags$table(
      tags$tr(
        tags$td(
          actionButton("optimize_thresholds_x",
            "Automatically select threshold",
            width = "100%"
          ),
          style = "width: 49%;"
        ),
        tags$td(HTML("&nbsp;with&nbsp;")),
        tags$td(
          selectInput("threshold_method_x", "", c(
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
          style = "width: 49%;"
        )
      )
    ),
    hr(class = "hr-text", `data-content` = "Or set it manually"),
    sliderInput("threshold_x", "Difference threshold",
      width = "100%", min = 1, max = 255,
      value = 30, step = 1
    ),
    hr(),
    htmlOutput("video_slider2")
  )
)
