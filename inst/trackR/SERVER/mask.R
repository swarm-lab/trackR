# Toggle panel
observeEvent(input$toggleMask, {
  if ("maskPanel" %in% input$bsCollapse) {
    updateCollapse(session, "bsCollapse", close = "maskPanel")
  } else {
    updateCollapse(session, "bsCollapse", open = "maskPanel",
                   close = c("backgroundPanel", "blobPanel", "videoPanel", "trackingPanel"))
    theActive("mask")
  }
})

# Select mask
shinyFileChoose(input, "maskFile", roots = getVolumes())

observeEvent(input$maskFile, {
  path <- parseFilePaths(roots = getVolumes(), input$maskFile)
  if (nrow(path) > 0) {
    toCheck <- tryCatch(Rvision::image(path$datapath),
                        error = function(e) NA)

    if (isImage(toCheck)) {
      theMask(toCheck)
    }
  }
})

output$maskStatus <- renderUI({
  if (isImage(theMask())) {
    p("Mask loaded.", class = "good")
  } else {
    p("Mask missing (but not required).", class = "bad")
  }
})

# Display mask
observe({
  if (theActive() == "mask") {
    if (isImage(theMask())) {
      if (is.null(input$videoSize)) {
        display(theMask(), "trackR", 25,
                nrow(theMask()),
                ncol(theMask()))
      } else {
        display(resize(theImage(), fx = input$videoQuality, fy = input$videoQuality,
                       interpolation = "area") *
                  (resize(theMask(), fx = input$videoQuality, fy = input$videoQuality,
                                                         interpolation = "area") / 255),
                "trackR", 25,
                nrow(theMask()) * input$videoSize,
                ncol(theMask()) * input$videoSize)
      }
    } else {
      if (is.null(input$videoSize)) {
        display(image(array(0, dim = c(640, 480, 3))), "trackR", 25, 480, 640)
      } else {
        display(image(array(0, dim = c(ncol(theImage()), nrow(theImage()), 3))),
                "trackR", 25,
                nrow(theImage()) * input$videoSize,
                ncol(theImage()) * input$videoSize)
      }
    }
  }
})
