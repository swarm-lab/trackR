theMaskPath <- reactiveVal()
refreshMask <- reactiveVal(0)

# Select mask
shinyFileChoose(input, "maskFile_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$maskFile_x, {
  path <- parseFilePaths(volumes, input$maskFile_x)
  if (nrow(path) > 0) {
    theMaskPath(path$datapath)
    refreshMask(refreshMask() + 1)
  }
})

observeEvent(refreshMask(), {
  if (refreshMask() > 0) {
    toCheck <- tryCatch(Rvision::image(theMaskPath()),
                        error = function(e) NA)

    if (Rvision::isImage(toCheck)) {
      theMask(Rvision::changeColorSpace(toCheck, "BGR"))
      ix <- sapply(volumes, grepl, x = theMaskPath())
      volume <- volumes[ix]
      dir <- dirname(theMaskPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))
    }
  }
})

# Display mask
observe({
  if (input$main == "3") {
    if (Rvision::isImage(theMask())) {
      displayMask <- Rvision::cloneImage(theMask())
      Rvision::setTo(displayMask, theMask(), "green", in_place = TRUE)
      Rvision::setTo(displayMask, Rvision::invert(theMask()), "red", in_place = TRUE)

      if (is.null(input$videoSize_x)) {
        Rvision::display(displayMask, "trackR", 25,
                         nrow(displayMask),
                         ncol(displayMask))
      } else {
        Rvision::display(
          Rvision::addWeighted(
            Rvision::resize(theImage(), fx = input$videoQuality_x,
                            fy = input$videoQuality_x,
                            interpolation = "area"),
            Rvision::resize(displayMask, fx = input$videoQuality_x,
                            fy = input$videoQuality_x,
                            interpolation = "area"),
            c(0.75, 0.25)
          ),
          "trackR", 25,
          nrow(displayMask) * input$videoSize_x,
          ncol(displayMask) * input$videoSize_x)
      }
    } else {
      if (is.null(input$videoSize_x)) {
        Rvision::display(Rvision::zeros(480, 640), "trackR", 25, 480, 640)
      } else {
        display(
          Rvision::zeros(nrow(theImage()), ncol(theImage())),
          "trackR", 25,
          nrow(theImage()) * input$videoSize_x,
          ncol(theImage()) * input$videoSize_x)
      }
    }
  }
})

# Add polygon
observeEvent(input$polyButton_x, {
  if (Rvision::isImage(theMask())) {
    toggleAll("OFF")

    displayMask <- Rvision::cloneImage(theMask())
    Rvision::setTo(displayMask, theMask(), "green", in_place = TRUE)
    Rvision::setTo(displayMask, Rvision::invert(theMask()), "red", in_place = TRUE)

    showNotification("Use left click to draw the ROI. Use right click to close
                       it and return the result.", id = "mask_notif", duration = NULL,
                     type = "message")

    if (is.null(input$videoSize_x)) {
      suppressMessages(ROI <- Rvision::selectROI(displayMask, "trackR", 1, TRUE))
    } else {
      suppressMessages(ROI <- Rvision::selectROI(
        Rvision::addWeighted(theImage(), displayMask, c(0.75, 0.25)),
        "trackR", input$videoSize_x, TRUE)
      )
    }

    removeNotification(id = "mask_notif")

    if (input$incButton_x == "Including") {
      theMask(Rvision::setTo(theMask(), ROI$mask, "white"))
    } else if (input$incButton_x == "Excluding") {
      theMask(Rvision::setTo(theMask(), ROI$mask, "black"))
    }

    toggleAll("ON")
  }
})

# Add Ellipse
observeEvent(input$ellButton_x, {
  if (Rvision::isImage(theMask())) {
    toggleAll("OFF")

    displayMask <- Rvision::cloneImage(theMask())
    Rvision::setTo(displayMask, theMask(), "green", in_place = TRUE)
    Rvision::setTo(displayMask, Rvision::invert(theMask()), "red", in_place = TRUE)

    showNotification("Select 5 points along the periphery of the ellipse/circle to define.",
                     id = "mask_notif", duration = NULL,
                     type = "message")

    ROI <- data.frame()

    if (is.null(input$videoSize_x)) {
      tmpImage <- Rvision::cloneImage(displayMask)
    } else {
      tmpImage <- Rvision::addWeighted(theImage(), displayMask, c(0.75, 0.25))
    }

    r <- 0.01 * min(nrow(tmpImage), ncol(tmpImage))

    for (i in 1:5) {
      if (is.null(input$videoSize_x)) {
        ROI <- rbind(ROI, Rvision::click(tmpImage, 1, "trackR"))
        Rvision::drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                            radius = r * 1.5, thickness = -1, color = "white")
        Rvision::drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                            radius = r, thickness = -1, color = "red")
        Rvision::display(tmpImage, window_name = "trackR", delay = 25,
                         height = nrow(tmpImage),  width = ncol(tmpImage))
      } else {
        ROI <- rbind(ROI, Rvision::click(tmpImage, input$videoSize_x, "trackR"))
        Rvision::drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                            radius = r * 1.5, thickness = -1, color = "white")
        Rvision::drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                            radius = r, thickness = -1, color = "red")
        Rvision::display(tmpImage, window_name = "trackR", delay = 25,
                         height = nrow(tmpImage) * input$videoSize_x,
                         width = ncol(tmpImage) * input$videoSize_x)
      }
    }

    removeNotification(id = "mask_notif")

    ell <- optimEllipse(ROI$x, ROI$y)
    ellMask <- Rvision::zeros(nrow(displayMask), ncol(displayMask))
    Rvision::drawEllipse(ellMask, ell[1], ell[2], ell[3] / 2, ell[4] / 2, ell[5],
                         color = "white", thickness = -1)

    if (input$incButton_x == "Including") {
      theMask(Rvision::setTo(theMask(), ellMask, "white"))
    } else if (input$incButton_x == "Excluding") {
      theMask(Rvision::setTo(theMask(), ellMask, "black"))
    }

    toggleAll("ON")
  }
})

# Include/exclude all
observeEvent(input$includeAll_x, {
  if (Rvision::isImage(theMask())) {
    theMask(Rvision::ones(nrow(theMask()), ncol(theMask())) * 255)
  }
})

observeEvent(input$excludeAll_x, {
  if (Rvision::isImage(theMask())) {
    theMask(Rvision::zeros(nrow(theMask()), ncol(theMask())))
  }
})


# Save mask
shinyFileSave(input, "saveMask_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$saveMask_x, {
  path <- parseSavePath(volumes, input$saveMask_x)

  if (Rvision::isImage(theMask()) & nrow(path) > 0) {
    Rvision::write.Image(theMask(), path$datapath)
    theMaskPath(path$datapath)
  }
})

# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "saveMask_x", "maskFile_x",
                     "polyButton_x", "incButton_x", "ellButton_x", "includeAll_x",
                     "excludeAll_x"))

onBookmark(function(state) {
  state$values$theMaskPath <- theMaskPath()
})

onRestore(function(state) {
  theMaskPath(state$values$theMaskPath[[1]])
})
