# Toggle panel
observeEvent(input$toggleTracking, {
  if ("trackingPanel" %in% input$bsCollapse) {
    updateCollapse(session, "bsCollapse", close = "trackingPanel")
  } else {
    updateCollapse(session, "bsCollapse", open = "trackingPanel",
                   close = c("backgroundPanel", "blobPanel", "maskPanel", "videoPanel"))
    theActive("tracking")
  }
})

# Run tracking
observeEvent(input$computeTracks, {
    isolate({
      out <- trackR:::pipeline(theVideo(), input$rangePos[1], input$rangePos[2],
                               theBackground(), theMask(), input$blobBlur,
                               input$blobThreshold, input$blobSize[1],
                               input$blobSize[2], input$lookBack, input$maxDist,
                               theBlobSizes, TRUE, input$showTracks,
                               input$videoQuality, input$videoSize)
      write_csv(out, paste0(thePath(), "tracks.csv"))
      theSuccess(TRUE)
    })
})

output$trackingStatus <- renderUI({
  if (theSuccess()) {
    isolate({ p(paste0("Tracks saved in ", thePath(), "tracks.csv"), class = "good") })
  }
})
