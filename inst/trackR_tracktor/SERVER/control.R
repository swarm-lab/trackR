# Video slider
output$videoSlider <- renderUI({
  if (isVideo(theVideo()) & !is.null(input$rangePos)) {
    sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                max = diff(input$rangePos) + 1, step = 1)
  } else {
    sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                max = 1, step = 1)
  }
})

# Save settings
shinyFileSave(input, "saveSettings", roots = getVolumes())

observeEvent(input$saveSettings, {
  tmp <- parseSavePath(roots = getVolumes(), input$saveSettings)
  isolate({
    if (nrow(tmp) > 0) {
      settings$quality <- input$videoQuality
      settings$size <- input$videoSize
      settings$range <- input$rangePos
      settings$smoothing <- input$blobBlur
      settings$threshold <- input$blobThreshold
      settings$blob_size <- input$blobSize
      settings$lookback <- input$lookBack
      settings$max_dist <- input$maxDist
      save(settings, file = tmp$datapath)
    }
  })
})

# Load settings
shinyFileChoose(input, "loadSettings", roots = getVolumes())

observeEvent(input$loadSettings, {
  path <- parseFilePaths(roots = getVolumes(), input$loadSettings)
  if (nrow(path) > 0) {
    load(path$datapath)

    isolate({
      if (!is.na(settings$path))
        thePath(settings$path)

      if (!is.na(settings$file))
        theFile(settings$file)

      if (!is.na(settings$quality))
        updateSliderInput(session, "videoQuality", value = settings$quality)

      if (!is.na(settings$size))
        updateSliderInput(session, "videoSize", value = settings$size)

      if (all(!is.na(settings$range)))
        updateSliderInput(session, "rangePos", value = settings$range)

      if (!is.na(settings$background)) {
        toCheck <- tryCatch(Rvision::image(settings$background),
                            error = function(e) NA)

        if (isImage(toCheck)) {
          theBackground(toCheck)
        }
      }

      if (!is.na(settings$mask)) {
        toCheck <- tryCatch(Rvision::image(settings$mask),
                            error = function(e) NA)

        if (isImage(toCheck)) {
          theMask(toCheck)
        }
      }

      if (!is.na(settings$smoothing))
        updateSliderInput(session, "blobBlur", value = settings$smoothing)

      if (!is.na(settings$threshold))
        updateSliderInput(session, "blobThreshold", value = settings$threshold)

      if (all(!is.na(settings$blob_size)))
        updateSliderInput(session, "blobSize", value = settings$blob_size)

      if (!is.na(settings$lookback))
        updateSliderInput(session, "lookBack", value = settings$lookback)

      if (!is.na(settings$max_dist))
        updateSliderInput(session, "maxDist", value = settings$max_dist)
    })
  }
})
