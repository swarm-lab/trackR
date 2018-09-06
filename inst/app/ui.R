shinyUI(fluidPage(

  titlePanel("trackR"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  fluidRow(
    column(
      12,

      bsCollapse(

        id = "bsCollapse",
        multiple = TRUE,
        open = "videoPanel",

        bsCollapsePanel(
          title = actionLink("toggleVideo", "Video file"),
          value = "videoPanel",
          shinyFilesButton("videoFile", "Select video file",
                           "Please select a video file", FALSE, class = "fullWidth"),
          htmlOutput("rangeSlider"),
          htmlOutput("videoSlider"),
          htmlOutput("displaySlider"),
          htmlOutput("videoStatus")
        ),

        bsCollapsePanel(
          title = actionLink("toggleBackground", "Background"),
          value = "backgroundPanel",
          sliderInput("backroundImages", "Number of images:", min = 0, max = 200,
                      value = 10, width = "100%"),
          selectInput("backgroundType", "Type:",
                      choices = c("Median" = "median", "Mean" = "mean"), width = "100%"),
          actionButton("computeBackground", "Estimate background", width = "100%"),
          htmlOutput("backgroundStatus", container = p, class = "good")
        ),

        bsCollapsePanel(
          title = actionLink("toggleMask", "Mask"),
          value = "maskPanel",
          shinyFilesButton("maskFile", "Select mask file",
                           "Please select a mask file", FALSE, class = "fullWidth"),
          htmlOutput("maskStatus")
        ),

        bsCollapsePanel(
          title = actionLink("toggleBlob", "Blob detection"),
          value = "blobPanel",
          sliderInput("blobThreshold", "Detection threshold:", min = 1, max = 255,
                      value = 20, width = "100%"),
          sliderInput("blobMinSize", "Minimum size:", min = 1, max = 300, value = 1,
                      width = "100%")
        ),

        bsCollapsePanel(
          title = actionLink("toggleTracking", "Tracking"),
          value = "trackingPanel",
          sliderInput("lookBack", "Look back (frames):", min = 1, max = 150,
                      value = 60, width = "100%"),
          sliderInput("maxDist", "Maximum distance (pixels):", min = 1,
                      max = 100, value = 10, width = "100%"),
          checkboxInput("showTracks", "Display track samples", FALSE),
          actionButton("computeTracks", "Start tracking"),
          htmlOutput("trackingStatus")
        )

      )
    )
  )

))
