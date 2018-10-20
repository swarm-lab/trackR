# Toggle panel
observeEvent(input$toggleVideo, {
  if ("videoPanel" %in% input$bsCollapse) {
    updateCollapse(session, "bsCollapse", close = "videoPanel")
  } else {
    updateCollapse(session, "bsCollapse", open = "videoPanel",
                   close = c("backgroundPanel", "blobPanel", "maskPanel", "trackingPanel"))
    theActive("video")
  }
})

# Select video
shinyFileChoose(input, "videoFile", roots = c(home = "~/"))

observeEvent(input$videoFile, {
  path <- parseFilePaths(roots = c(home = "~/"), input$videoFile)
  if (nrow(path) > 0) {
    theFile(path$name)
    thePath(gsub(path$name, "", path$datapath))
  }
})

observe({
  if (length(thePath()) > 0) {
    toCheck <- tryCatch(Rvision::video(paste0(thePath(), theFile())),
                    error = function(e) NA)

    if (isVideo(toCheck)) {
      if (!is.na(nframes(toCheck))) {
        theVideo(toCheck)
        isolate(theImage(readFrame(theVideo(), 1)))
      }
    }
  }
})

output$videoStatus <- renderUI({
  if (isVideo(theVideo())) {
    p("Video loaded.", class = "good")
  } else {
    p("Video missing (and required).", class = "bad")
  }
})

# Display video
output$rangeSlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("rangePos", "Video range", width = "100%", min = 1, max = nframes(theVideo()),
                value = c(0, nframes(theVideo())), step = 1)
  }
})

output$displaySlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("videoSize", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$qualitySlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("videoQuality", "Video quality", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

observe({
  if (isVideo(theVideo()) & !is.null(input$videoPos) & !is.null(input$rangePos) & !is.null(input$videoQuality)) {
    isolate({
      theImage(readFrame(theVideo(), input$videoPos + input$rangePos[1] - 1))
    })
  }
})

observe({
  if (theActive() == "video") {
    if (isImage(theImage()) & !is.null(input$videoSize) & theActive() == "video") {
      display(resize(theImage(), fx = input$videoQuality, fy = input$videoQuality,
                     interpolation = "area"),
              "trackR", 25,
              nrow(theImage()) * input$videoSize,
              ncol(theImage()) * input$videoSize)
    } else {
      display(image(array(0, dim = c(640, 480, 3))), "trackR", 25,
              480,
              640)
    }
  }
})
