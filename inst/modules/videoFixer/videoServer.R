volumes <- c(Home = fs::path_home(), getVolumes()())
colNames <- c("x", "y", "n", "frame", "track", "width", "height", "angle")

theVideoPath <- reactiveVal()
theVideo <- reactiveVal()
theImage <- reactiveVal()
defaultRoot <- reactiveVal(NULL)
defaultPath <- reactiveVal("")

# Select video
shinyFileChoose(input, "videoFile_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$videoFile_x, {
  path <- parseFilePaths(volumes, input$videoFile_x)
  if (nrow(path) > 0) {
    theVideoPath(path$datapath)
  }
})

observeEvent(theVideoPath(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(theVideoPath(), volumes),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      })
  )
  # ix <- sapply(volumes, grepl, x = theVideoPath())
  volume <- volumes[ix]
  dir <- dirname(theVideoPath())
  defaultRoot(names(volumes)[ix])
  defaultPath(gsub(volume, "", dir))
})

observeEvent(theVideoPath(), {
  toCheck <- tryCatch(Rvision::video(theVideoPath()),
                      error = function(e) NA)

  if (Rvision::isVideo(toCheck)) {
    if (!is.na(Rvision::nframes(toCheck))) {
      theVideo(toCheck)
    }
  }
})

output$videoStatus <- renderUI({
  if (!Rvision::isVideo(theVideo())) {
    p("Video missing (and required).", class = "bad")
  }
})

# Controls
output$displaySlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$rangeSlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    tagList(
      hr(),
      sliderInput("rangePos_x", "Select video range", width = "100%", min = 1,
                  max = Rvision::nframes(theVideo()),
                  value = c(1, Rvision::nframes(theVideo())), step = 1)
    )
  }
})

rangeMem <- c(NA, NA)
output$videoSlider <- renderUI({
  if (Rvision::isVideo(theVideo()) & !is.null(input$rangePos_x)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      sliderInput("videoPos_x", "Select a reference frame", width = "100%",
                  value = diff(input$rangePos_x) + 1,
                  min = 1, max = diff(input$rangePos_x) + 1, step = 1)
    } else {
      sliderInput("videoPos_x", "Select a reference frame", width = "100%",
                  value = 1, min = 1, max = diff(input$rangePos_x) + 1, step = 1)
    }
  }
})

observe({
  if (!is.null(input$videoPos_x) & !is.null(input$videoSize_x) & !is.null(input$rangePos_x))
    theImage(Rvision::readFrame(theVideo(), input$videoPos_x + input$rangePos_x[1] - 1))
})

# Display video
observeEvent(theImage(), {
  if (Rvision::isImage(theImage())) {
    Rvision::display(theImage(), "videoFixer", 5,
                     nrow(theImage()) * input$videoSize_x,
                     ncol(theImage()) * input$videoSize_x)
  } else {
    Rvision::display(Rvision::zeros(480, 640), "videoFixer", 5, 480, 640)
  }
})
