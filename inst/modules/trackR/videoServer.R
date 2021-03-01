volumes <- c(Home = fs::path_home(), getVolumes()())
theVideoPath <- reactiveVal()
theVideo <- reactiveVal()
theFrame <- reactiveVal()
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
  volume <- volumes[ix]

  if (length(volume) > 0) {
    dir <- dirname(theVideoPath())
    defaultRoot(names(volumes)[ix])
    defaultPath(gsub(volume, "", dir))
  }
})

observeEvent(theVideoPath(), {
  toCheck <- tryCatch(video(theVideoPath()),
                      error = function(e) NA)

  if (isVideo(toCheck)) {
    if (!is.na(nframes(toCheck))) {
      theVideo(toCheck)
      theImage(readFrame(theVideo(), 1))
      theMask(ones(nrow(theVideo()), ncol(theVideo())) * 255)
    }
  }
})

output$videoStatus <- renderUI({
  if (!isVideo(theVideo())) {
    p("Video missing (and required).", class = "bad")
  }
})

# Display video
output$rangeSlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                max = nframes(theVideo()),
                value = c(1, nframes(theVideo())), step = 1)
  }
})

output$displaySlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$qualitySlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("videoQuality_x", "Video quality", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

observeEvent(theFrame(), {
  if (!is.null(theFrame()))
    theImage(readFrame(theVideo(), theFrame()))
})

observe({
  if (input$main == "1") {
    if (isImage(theImage()) & !is.null(input$videoSize_x)) {
      display(
        resize(theImage(), fx = input$videoQuality_x,
               fy = input$videoQuality_x, interpolation = "area"),
        "trackR", 5,
        nrow(theImage()) * input$videoSize_x,
        ncol(theImage()) * input$videoSize_x)
    } else {
      display(zeros(480, 640), "trackR", 5, 480, 640)
    }
  }
})

rangeMem <- c(NA, NA)
frameMem <- NA

output$videoSlider <- renderUI({
  if (isVideo(theVideo()) & !is.null(input$rangePos_x)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      sliderInput("videoPos_x", "Frame", width = "100%", step = 1,
                  value = input$rangePos_x[2],
                  min = input$rangePos_x[1],
                  max = input$rangePos_x[2])
    } else {
      sliderInput("videoPos_x", "Frame", width = "100%", step = 1,
                  value = input$rangePos_x[1],
                  min = input$rangePos_x[1],
                  max = input$rangePos_x[2])
    }
  }
})

observeEvent(input$videoPos_x, {
  theFrame(input$videoPos_x)
  frameMem <<- input$videoPos_x

  if (input$main == "1") {
    if (!is.null(input$videoPos2_x))
      updateSliderInput(session, "videoPos2_x", value = input$videoPos_x)

    if (!is.null(input$videoPos3_x))
      updateSliderInput(session, "videoPos3_x", value = input$videoPos_x)
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
