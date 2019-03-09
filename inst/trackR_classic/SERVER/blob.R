# Toggle panel
observeEvent(input$toggleBlob, {
  if ("blobPanel" %in% input$bsCollapse) {
    updateCollapse(session, "bsCollapse", close = "blobPanel")
  } else {
    updateCollapse(session, "bsCollapse", open = "blobPanel",
                   close = c("backgroundPanel", "videoPanel", "maskPanel", "trackingPanel"))
    theActive("blob")
  }
})

# Parameterize blob detection
tmpBlobSizes <- {}

observe({
  if (!is.null(input$blobThreshold) & !is.null(input$blobSize) &
      !is.null(input$blobBlur) & isImage(theImage()) & theActive() == "blob") {
    if (input$videoQuality < 1) {
      isolate(theBlobs(resize(theImage(), fx = input$videoQuality,
                              fy = input$videoQuality,
                              interpolation = "area")))
      if (isImage(theBackground())) {
        if (isImage(theMask())) {
          cc <- trackR:::blobClassic(resize(theImage(), fx = input$videoQuality,
                                        fy = input$videoQuality,
                                        interpolation = "area"),
                                 resize(theBackground(), fx = input$videoQuality,
                                        fy = input$videoQuality,
                                        interpolation = "area"),
                                 resize(theMask(), fx = input$videoQuality,
                                        fy = input$videoQuality,
                                        interpolation = "area"),
                                 input$blobBlur, input$blobThreshold)
        } else {
          cc <- trackR:::blobClassic(resize(theImage(), fx = input$videoQuality,
                                        fy = input$videoQuality,
                                        interpolation = "area"),
                                 resize(theBackground(), fx = input$videoQuality,
                                        fy = input$videoQuality,
                                        interpolation = "area"),
                                 NULL, input$blobBlur, input$blobThreshold)
        }
      } else {
        cc$table <- data.frame()
      }
    } else {
      isolate(theBlobs(cloneImage(theImage())))
      if (isImage(theBackground())) {
        if (isImage(theMask())) {
          cc <- trackR:::blobClassic(theImage(), theBackground(), theMask(), input$blobBlur, input$blobThreshold)
        } else {
          cc <- trackR:::blobClassic(theImage(), theBackground(), NULL, input$blobBlur, input$blobThreshold)
        }
      } else {
        cc$table <- data.frame()
      }
    }

    if (nrow(cc$table) > 0) {
      ccc <- do.call(rbind, by(cc$table, cc$table[, "id"],
                               function(x) data.frame(id = x[1, "id"],
                                                      n = nrow(x),
                                                      x = mean(x[, "x"]),
                                                      y = mean(x[, "y"]))))

      tmpBlobSizes <<- ccc$n[ccc$n >= input$blobSize[1] & ccc$n <= input$blobSize[2]]
      toContour <- cc$labels > 0
      exclude <- ccc$id[ccc$n < input$blobSize[1] | ccc$n > input$blobSize[2]]
      for (i in seq_len(length(exclude))) {
        toContour <- toContour - (cc$labels == exclude[i])
      }

      theContours <- findContours(toContour)

      isolate({

        invisible(by(theContours$contours, theContours$contours[, "id"],
                     function(x, img) {
                       if (nrow(x) > 1) {
                         id1 <- c(nrow(x), 1:(nrow(x) - 1))
                         id2 <- c(2:nrow(x), 1)

                         drawLine(img, x[id1, "x"], -x[id1, "y"] + nrow(img),
                                  x[id2, "x"], -x[id2, "y"] + nrow(img),
                                  thickness = 3)
                       }

                     },
                     img = theBlobs()))
      })
    }
  }
})

observeEvent(input$recordBlobs, {
  theBlobSizes <<- c(theBlobSizes, tmpBlobSizes)
  showNotification("Blobs recorded.", id = "load", duration = 2)
})

# Display blobs
observe({
  if (theActive() == "blob") {
    if (isImage(theBlobs())) {
      if (is.null(input$videoSize)) {
        display(theBlobs(), "trackR", 25,
                nrow(theImage()),
                ncol(theImage()))
      } else {
        display(theBlobs(), "trackR", 25,
                nrow(theImage()) * input$videoSize,
                ncol(theImage()) * input$videoSize)
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
