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

      if (!all(dim(toCheck) == dim(theImage))) {
        shinyalert("Error:",
                   "The video and mask do not have the same dimensions.",
                   type = "error", animation = FALSE,
                   closeOnClickOutside = TRUE)
        theMask <<- NULL
      } else {
        theMask <<- cloneImage(toCheck)
      }


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

      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "3") {
    if (isImage(theMask)) {
      if (isImage(theImage)) {
        toDisplay <- ones(nrow(theImage), ncol(theImage), 3)
      } else {
        toDisplay <- zeros(nrow(theMask), ncol(theMask), 3)
      }

      setTo(toDisplay, changeColorSpace(theMask, "GRAY"), "green", target = "self")
      setTo(toDisplay, invert(changeColorSpace(theMask * 255, "GRAY")), "red", target = "self")

      if (isImage(theImage)) {
        addWeighted(toDisplay, theImage, c(0.25, 0.75), target = toDisplay)
      } else {
        addWeighted(toDisplay, zeros(nrow(theMask), ncol(theMask), 3),
                    c(0.25, 0.75), target = toDisplay)
      }

      setTo(toDisplay, canny(theMask, 0, 0), "white", target = "self")

      ch1 <- extractChannel(theMask, 1)
      h <- imhist(ch1, nbins = 257, range = c(0, 256))
      valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]

      com <- lapply(valid, function(val) {
        cc <- connectedComponents(ch1 == val, stats = FALSE)
        lapply(1:cc$n, function(lab) {
          dt <- distanceTransform(border(cc$labels == lab, 1))
          mm <- minMaxLoc(dt)
          mm[2, 2:3] - 1
        })
      })

      lab <- lapply(com, function(l) {
        lapply(l, function(loc) {
          pget(theMask, loc[1], loc[2])[1]
        })
      })

      sc <- max(dim(toDisplay) / 720)

      mapply(function(com, lab) {
        mapply(function(com, lab) {
          drawText(toDisplay, lab,
                   com[1] - (floor(log10(lab)) + 1) * 5,
                   com[2] - 5 * sc,
                   font_scale = 0.5 * sc,
                   thickness = max(1, 1.5 * sc),
                   color = "white")
        }, com = com, lab = lab)
      }, com = com, lab = lab)
    } else if (isImage(theImage)) {
      theMask <<- ones(nrow(theImage), ncol(theImage), 3)
      toDisplay <- ones(nrow(theImage), ncol(theImage), 3)
      setTo(toDisplay, changeColorSpace(theMask, "GRAY"), "green", target = "self")
      addWeighted(toDisplay, theImage, c(0.25, 0.75), target = toDisplay)

      setTo(toDisplay, canny(theMask, 0, 0), "white", target = "self")

      ch1 <- extractChannel(theMask, 1)
      h <- imhist(ch1, nbins = 257, range = c(0, 256))
      valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]

      com <- lapply(valid, function(val) {
        cc <- connectedComponents(ch1 == val, stats = FALSE)
        lapply(1:cc$n, function(lab) {
          dt <- distanceTransform(border(cc$labels == lab, 1))
          mm <- minMaxLoc(dt)
          mm[2, 2:3] - 1
        })
      })

      lab <- lapply(com, function(l) {
        lapply(l, function(loc) {
          pget(theMask, loc[1], loc[2])[1]
        })
      })

      sc <- max(dim(toDisplay) / 720)

      mapply(function(com, lab) {
        mapply(function(com, lab) {
          drawText(toDisplay, lab,
                   com[1] - (floor(log10(lab)) + 1) * 5,
                   com[2] - 5 * sc,
                   font_scale = 0.5 * sc,
                   thickness = max(1, 1.5 * sc),
                   color = "white")
        }, com = com, lab = lab)
      }, com = com, lab = lab)
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
    setTo(displayMask, invert(changeColorSpace(theMask * 255, "GRAY")), "red", target = "self")

    setTo(displayMask, canny(theMask, 0, 0), "white", target = "self")

    ch1 <- extractChannel(theMask, 1)
    h <- imhist(ch1, nbins = 257, range = c(0, 256))
    valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]

    com <- lapply(valid, function(val) {
      cc <- connectedComponents(ch1 == val, stats = FALSE)
      lapply(1:cc$n, function(lab) {
        dt <- distanceTransform(border(cc$labels == lab, 1))
        mm <- minMaxLoc(dt)
        mm[2, 2:3] - 1
      })
    })

    lab <- lapply(com, function(l) {
      lapply(l, function(loc) {
        pget(theMask, loc[1], loc[2])[1]
      })
    })

    sc <- max(dim(displayMask) / 720)

    mapply(function(com, lab) {
      mapply(function(com, lab) {
        drawText(displayMask, lab,
                 com[1] - (floor(log10(lab)) + 1) * 5,
                 com[2] - 5 * sc,
                 font_scale = 0.5 * sc,
                 thickness = max(1, 1.5 * sc),
                 color = "white")
      }, com = com, lab = lab)
    }, com = com, lab = lab)

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
      setTo(theMask, ROI$mask,
            rgb(input$roi_x, input$roi_x, input$roi_x, maxColorValue = 255),
            target = "self")
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
    setTo(displayMask, invert(changeColorSpace(theMask * 255, "GRAY")), "red", target = "self")

    setTo(displayMask, canny(theMask, 0, 0), "white", target = "self")

    ch1 <- extractChannel(theMask, 1)
    h <- imhist(ch1, nbins = 257, range = c(0, 256))
    valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]

    com <- lapply(valid, function(val) {
      cc <- connectedComponents(ch1 == val, stats = FALSE)
      lapply(1:cc$n, function(lab) {
        dt <- distanceTransform(border(cc$labels == lab, 1))
        mm <- minMaxLoc(dt)
        mm[2, 2:3] - 1
      })
    })

    lab <- lapply(com, function(l) {
      lapply(l, function(loc) {
        pget(theMask, loc[1], loc[2])[1]
      })
    })

    sc <- max(dim(displayMask) / 720)

    mapply(function(com, lab) {
      mapply(function(com, lab) {
        drawText(displayMask, lab,
                 com[1] - (floor(log10(lab)) + 1) * 5,
                 com[2] - 5 * sc,
                 font_scale = 0.5 * sc,
                 thickness = max(1, 1.5 * sc),
                 color = "white")
      }, com = com, lab = lab)
    }, com = com, lab = lab)

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
      setTo(theMask, ellMask,
            rgb(input$roi_x, input$roi_x, input$roi_x, maxColorValue = 255),
            target = "self")
    } else if (input$incButton_x == "Excluding") {
      setTo(theMask, ellMask, "black", target = "self")
    }

    toggleAll("ON")
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$incButton_x, {
  if (input$incButton_x == "Including") {
    shinyjs::enable("roi_x")
  } else {
    shinyjs::disable("roi_x")
  }
})

observeEvent(input$includeAll_x, {
  if (isImage(theMask)) {
    theMask <<- ones(nrow(theMask), ncol(theMask), 3)
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
    write.Image(theMask, path$datapath, TRUE)
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
