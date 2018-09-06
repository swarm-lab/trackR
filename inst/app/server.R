shinyServer(function(input, output, session) {

  thePath <- reactiveVal()
  theFile <- reactiveVal()
  theVideo <- reactiveVal()
  theImage <- reactiveVal()
  theBackground <- reactiveVal()
  theMask <- reactiveVal(NULL)
  theBlobs <- reactiveVal()
  theSuccess <- reactiveVal(FALSE)

  # Toggle panels
  observeEvent(input$toggleVideo, {
    if ("videoPanel" %in% input$bsCollapse) {
      updateCollapse(session, "bsCollapse", close = "videoPanel")
    } else {
      updateCollapse(session, "bsCollapse", open = "videoPanel")
    }
  })

  observeEvent(input$toggleBackground, {
    if ("backgroundPanel" %in% input$bsCollapse) {
      updateCollapse(session, "bsCollapse", close = "backgroundPanel")
    } else {
      updateCollapse(session, "bsCollapse", open = "backgroundPanel")
    }
  })

  observeEvent(input$toggleMask, {
    if ("maskPanel" %in% input$bsCollapse) {
      updateCollapse(session, "bsCollapse", close = "maskPanel")
    } else {
      updateCollapse(session, "bsCollapse", open = "maskPanel")
    }
  })

  observeEvent(input$toggleBlob, {
    if ("blobPanel" %in% input$bsCollapse) {
      updateCollapse(session, "bsCollapse", close = "blobPanel")
    } else {
      updateCollapse(session, "bsCollapse", open = "blobPanel")
    }
  })

  observeEvent(input$toggleTracking, {
    if ("trackingPanel" %in% input$bsCollapse) {
      updateCollapse(session, "bsCollapse", close = "trackingPanel")
    } else {
      updateCollapse(session, "bsCollapse", open = "trackingPanel")
    }
  })

  # Select video
  shinyFileChoose(input, "videoFile", roots = c(home = "~/"))

  observeEvent(input$videoFile, {
    tmp <- parseFilePaths(roots = c(home = "~/"), input$videoFile)
    if (nrow(tmp) > 0) {
      theFile(tmp$name)
      thePath(gsub(tmp$name, "", tmp$datapath))
    }
  })

  observe({
    if (length(thePath()) > 0) {
      theVideo(Rvision::video(paste0(thePath(), theFile())))
      isolate(theImage(readFrame(theVideo(), 1)))
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
  output$videoSlider <- renderUI({
    if (isVideo(theVideo())) {
      sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                  max = diff(input$rangePos) + 1, step = 1)
    }
  })

  output$rangeSlider <- renderUI({
    if (isVideo(theVideo())) {
      sliderInput("rangePos", "Video range", width = "100%", min = 1, max = nframes(theVideo()),
                  value = c(0, nframes(theVideo())), step = 1)
    }
  })

  output$displaySlider <- renderUI({
    if (isVideo(theVideo())) {
      sliderInput("videoSize", "Display size", width = "100%", value = 1,
                  min = 0, max = 1, step = 0.1)
    }
  })

  observe({
    if (isImage(theImage()) & !is.null(input$videoSize)) {
      display(theImage(), "trackR", 25,
              nrow(theImage()) * input$videoSize,
              ncol(theImage()) * input$videoSize)
    }
  })

  # Compute background
  observe({
    if (length(thePath()) > 0) {
      if (file.exists(paste0(thePath(), "bg.png"))) {
        theBackground(image(paste0(thePath(), "bg.png")))
      }
    }
  })

  observe({
    if (input$computeBackground > 0) {
      isolate({
        theBackground(backgrounder(theVideo(), n = input$backroundImages,
                                   method = input$backgroundType))
        write.Image(theBackground(), paste0(thePath(), "bg.png"))
      })
    }
  })

  output$backgroundStatus <- renderUI({
    if (isImage(theBackground())) {
      p("Background loaded.", class = "good")
    } else {
      p("Background missing (and required).", class = "bad")
    }
  })

  # Select mask
  shinyFileChoose(input, "maskFile", roots = c(home = "~/"))

  observe({
    if (length(thePath()) > 0) {
      if (file.exists(paste0(thePath(), "mask.png"))) {
        theMask(image(paste0(thePath(), "mask.png")))
      }
    }
  })

  observeEvent(input$maskFile, {
    tmp <- parseFilePaths(roots = c(home = "~/"), input$maskFile)
    if (nrow(tmp) > 0) {
      theMask(image(tmp$datapath))
    }
  })

  output$maskStatus <- renderUI({
    if (isImage(theMask())) {
      p("Mask loaded.", class = "good")
    } else {
      p("Mask missing (but not required).", class = "bad")
    }
  })

  # Parameterize blob detection
  observe({
    if (!is.null(input$blobThreshold) & !is.null(input$blobMinSize) &
        isVideo(theVideo()) & isImage(theBackground()) &
        !is.null(input$videoPos) & !is.null(input$rangePos)) {
      theMask()
      isolate({
        theImage(readFrame(theVideo(), input$videoPos + input$rangePos[1] - 1))
        theBlobs(findBlobs(theImage(), theBackground(), theMask(),
                           threshold = input$blobThreshold,
                           minSize = input$blobMinSize))
        drawCircle(theImage(), x = theBlobs()$x, y = theBlobs()$y,
                   radius = theBlobs()$size, thickness = 2)
      })
    }
  })

  # Run tracking
  observe({
    if (input$computeTracks > 0) {
      isolate({
        out <- pipeline(theVideo(), input$rangePos[1], input$rangePos[2], theBackground(),
                        theMask(), input$blobThreshold, input$blobMinSize,
                        input$lookBack, input$maxDist, TRUE, input$showTracks,
                        input$videoSize)
        write_csv(out, paste0(thePath(), "track.csv"))
        theSuccess(TRUE)
      })
    }
  })

  output$trackingStatus <- renderUI({
    if (theSuccess()) {
      p("Tracking finished.", class = "good")
    }
  })

  # Clean up
  session$onSessionEnded(function() {
    destroyAllDisplays()
  })

})
