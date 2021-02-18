volumes <- c(Home = fs::path_home(), getVolumes()())

theVideoPath <- reactiveVal()
theVideo <- reactiveVal()
theImage <- reactiveVal()
theMask <- reactiveVal()
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
  ix <- sapply(volumes, grepl, x = theVideoPath())
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
      theImage(Rvision::readFrame(theVideo(), 1))
      theMask(Rvision::ones(nrow(theVideo()), ncol(theVideo())) * 255)
    }
  }
})

output$videoStatus <- renderUI({
  if (!Rvision::isVideo(theVideo())) {
  #   p("Video loaded.", class = "good")
  # } else {
    p("Video missing (and required).", class = "bad")
  }
})

# Display video
output$rangeSlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                max = Rvision::nframes(theVideo()),
                value = c(1, Rvision::nframes(theVideo())), step = 1)
  }
})

output$displaySlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$qualitySlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoQuality_x", "Video quality", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

observe({
  if (Rvision::isVideo(theVideo()) & !is.null(input$videoPos_x) &
      !is.null(input$rangePos_x) & !is.null(input$videoQuality_x)) {
    isolate({
      theImage(Rvision::readFrame(theVideo(), input$videoPos_x + input$rangePos_x[1] - 1))
    })
  }
})

observe({
  if (input$main == "1") {
    if (Rvision::isImage(theImage()) & !is.null(input$videoSize_x)) {
      Rvision::display(
        Rvision::resize(theImage(), fx = input$videoQuality_x,
                        fy = input$videoQuality_x, interpolation = "area"),
        "trackR", 25,
        nrow(theImage()) * input$videoSize_x,
        ncol(theImage()) * input$videoSize_x)
    } else {
      Rvision::display(Rvision::zeros(480, 640), "trackR", 25, 480, 640)
    }
  }
})

rangeMem <- c(NA, NA)

# Video slider
output$videoSlider <- renderUI({
  if (Rvision::isVideo(theVideo()) & !is.null(input$rangePos_x)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      sliderInput("videoPos_x", "Frame", width = "100%",
                  value = diff(input$rangePos_x) + 1,
                  min = 1, max = diff(input$rangePos_x) + 1, step = 1)
    } else {
      sliderInput("videoPos_x", "Frame", width = "100%", value = 1, min = 1,
                  max = diff(input$rangePos_x) + 1, step = 1)
    }
  }
})

# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "videoFile_x",
                     "videoFile_x-modal"))

onBookmark(function(state) {
  state$values$theVideoPath <- theVideoPath()
})

onRestore(function(state) {
  theVideoPath(state$values$theVideoPath[[1]])
})
