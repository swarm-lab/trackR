# Variables and reactives
theMask <- NULL

theMaskPath <- reactiveVal()
refreshMask <- reactiveVal(0)


# Outputs


# Events
observeEvent(input$main, {
  if (input$main == "3") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

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
    toCheck <- tryCatch(image(theMaskPath()), error = function(e) NA)

    if (isImage(toCheck)) {
      if (colorspace(toCheck) != "BGR")
        changeColorSpace(toCheck, "BGR", "self")

      theMask <<- cloneImage(toCheck)
      refreshDisplay(refreshDisplay() + 1)

      ix <- which.max(
        sapply(
          stringr::str_locate_all(theMaskPath(), volumes),
          function(l) {
            if (nrow(l) > 0) {
              diff(l[1, ])
            } else {
              NA
            }
          })
      )
      volume <- volumes[ix]
      dir <- dirname(theMaskPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))
    }
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "3") {
    if (isImage(theMask)) {
      toDisplay <- ones(nrow(theImage), ncol(theImage), 3)
      setTo(toDisplay, changeColorSpace(theMask, "GRAY"), "green", target = "self")
      setTo(toDisplay, invert(changeColorSpace(theMask, "GRAY")), "red", target = "self")
      addWeighted(toDisplay, theImage, c(0.25, 0.75), target = toDisplay)
    } else if (isImage(theImage)) {
      theMask <<- ones(nrow(theImage), ncol(theImage), 3)
      theMask %i*% 255
      toDisplay <- ones(nrow(theImage), ncol(theImage), 3)
      setTo(toDisplay, changeColorSpace(theMask, "GRAY"), "green", target = "self")
      addWeighted(toDisplay, theImage, c(0.25, 0.75), target = toDisplay)
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

observeEvent(input$polyButton_x, {
  if (isImage(theMask)) {
    toggleAll("OFF")

    displayMask <- ones(nrow(theMask), ncol(theMask), 3)
    setTo(displayMask, changeColorSpace(theMask, "GRAY"), "green", target = "self")
    setTo(displayMask, invert(changeColorSpace(theMask, "GRAY")), "red", target = "self")

    showNotification("Use left click to draw the ROI. Use right click to close
                       it and return the result.", id = "mask_notif", duration = NULL,
                     type = "message")

    if (is.null(input$videoSize_x)) {
      suppressMessages(ROI <- selectROI(displayMask, "trackR", 1, TRUE))
    } else {
      suppressMessages(ROI <- selectROI(
        addWeighted(theImage, displayMask, c(0.75, 0.25)),
        "trackR", input$videoSize_x, TRUE)
      )
    }

    removeNotification(id = "mask_notif")

    if (input$incButton_x == "Including") {
      setTo(theMask, ROI$mask, "white", target = "self")
    } else if (input$incButton_x == "Excluding") {
      setTo(theMask, ROI$mask, "black", target = "self")
    }

    toggleAll("ON")
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$ellButton_x, {
  if (isImage(theMask)) {
    toggleAll("OFF")

    displayMask <- ones(nrow(theMask), ncol(theMask), 3)
    setTo(displayMask, changeColorSpace(theMask, "GRAY"), "green", target = "self")
    setTo(displayMask, invert(changeColorSpace(theMask, "GRAY")), "red", target = "self")

    showNotification("Select 5 points along the periphery of the ellipse/circle to define.",
                     id = "mask_notif", duration = NULL,
                     type = "message")

    ROI <- data.frame()

    if (is.null(input$videoSize_x)) {
      tmpImage <- cloneImage(displayMask)
    } else {
      tmpImage <- addWeighted(theImage, displayMask, c(0.75, 0.25))
    }

    r <- 0.01 * min(nrow(tmpImage), ncol(tmpImage))

    for (i in 1:5) {
      if (is.null(input$videoSize_x)) {
        ROI <- rbind(ROI, Rvision::click(tmpImage, 1, "trackR"))
        drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                   radius = r * 1.5, thickness = -1, color = "white")
        drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                   radius = r, thickness = -1, color = "red")
        display(tmpImage, window_name = "trackR", delay = 25,
                height = nrow(tmpImage),  width = ncol(tmpImage))
      } else {
        ROI <- rbind(ROI, Rvision::click(tmpImage, input$videoSize_x, "trackR"))
        drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                   radius = r * 1.5, thickness = -1, color = "white")
        drawCircle(tmpImage, x = ROI$x[nrow(ROI)], y = ROI$y[nrow(ROI)],
                   radius = r, thickness = -1, color = "red")
        display(tmpImage, window_name = "trackR", delay = 25,
                height = nrow(tmpImage) * input$videoSize_x,
                width = ncol(tmpImage) * input$videoSize_x)
      }
    }

    removeNotification(id = "mask_notif")

    ell <- optimEllipse(ROI$x, ROI$y)
    ellMask <- zeros(nrow(displayMask), ncol(displayMask), 1)
    drawEllipse(ellMask, ell[1], ell[2], ell[3] / 2, ell[4] / 2, ell[5],
                color = "white", thickness = -1)

    if (input$incButton_x == "Including") {
      setTo(theMask, ellMask, "white", target = "self")
    } else if (input$incButton_x == "Excluding") {
      setTo(theMask, ellMask, "black", target = "self")
    }

    toggleAll("ON")
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$includeAll_x, {
  if (isImage(theMask)) {
    theMask <<- ones(nrow(theMask), ncol(theMask), 3)
    theMask %i*% 255
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$excludeAll_x, {
  if (isImage(theMask)) {
    theMask <<- zeros(nrow(theMask), ncol(theMask), 3)
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFileSave(input, "saveMask_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$saveMask_x, {
  path <- parseSavePath(volumes, input$saveMask_x)

  if (isImage(theMask) & nrow(path) > 0) {
    write.Image(theMask, path$datapath)
    theMaskPath(path$datapath)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "saveMask_x", "maskFile_x",
                     "polyButton_x", "incButton_x", "ellButton_x", "includeAll_x",
                     "excludeAll_x", "refreshMask"))

onBookmark(function(state) {
  state$values$theMaskPath <- theMaskPath()
})

onRestore(function(state) {
  theMaskPath(state$values$theMaskPath[[1]])
  refreshMask(refreshMask() + 1)
})
