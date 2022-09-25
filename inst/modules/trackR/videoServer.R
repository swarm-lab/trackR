# Variables and reactives
volumes <- c(Home = fs::path_home(), getVolumes()())
theVideo <- NULL
theImage <- NULL
rangeMem <- c(NA, NA)
frameMem <- NA

theVideoPath <- reactiveVal()
theFrame <- reactiveVal()
defaultRoot <- reactiveVal(NULL)
defaultPath <- reactiveVal("")
refreshVideo <- reactiveVal(0)
refreshDisplay <- reactiveVal(0)


# Outputs
output$videoStatus <- renderUI({
  if (refreshDisplay() > -1 & !isVideo(theVideo)) {
    p("Video missing (and required).", class = "bad")
  }
})

output$rangeSlider <- renderUI({
  if (refreshVideo() > 0 & isVideo(theVideo)) {
    sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                max = nframes(theVideo),
                value = c(1, nframes(theVideo)), step = 1)
  }
})

output$displaySlider <- renderUI({
  if (refreshVideo() > 0 & isVideo(theVideo)) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$qualitySlider <- renderUI({
  if (refreshVideo() > 0 & isVideo(theVideo)) {
    sliderInput("videoQuality_x", "Video quality", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$videoSlider <- renderUI({
  if (!is.null(input$rangePos_x) & isVideo(theVideo)) {
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


# Events
observeEvent(input$main, {
  if (input$main == "1") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

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
  toCheck <- tryCatch(video(theVideoPath()), error = function(e) NA)

  if (isVideo(toCheck)) {
    if (!is.na(nframes(toCheck))) {
      theVideo <<- toCheck
      theImage <<- readFrame(theVideo, 1)
      refreshVideo(refreshVideo() + 1)
      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

observeEvent(input$videoSize_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$videoQuality_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(theFrame(), {
  if (!is.null(theFrame())) {
    readFrame(theVideo, theFrame(), theImage)
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "1") {
    if (isImage(theImage)) {
      toDisplay <- cloneImage(theImage)
    } else {
      toDisplay <- zeros(480, 640, 3)
    }

    if (is.null(input$videoSize_x)) {
      display(toDisplay, "trackR", 5, nrow(toDisplay), ncol(toDisplay))
    } else {
      display(toDisplay, "trackR", 5,
              nrow(toDisplay) * input$videoSize_x,
              ncol(toDisplay) * input$videoSize_x)
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
setBookmarkExclude(c(session$getBookmarkExclude(), "refreshVideo", "refreshDisplay",
                     "videoFile_x", "videoFile_x-modal"))

onBookmark(function(state) {
  state$values$theVideoPath <- theVideoPath()
})

onRestore(function(state) {
  theVideoPath(state$values$theVideoPath[[1]])
})
