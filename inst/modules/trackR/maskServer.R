# Variables and reactives
theMask <- NULL
theMaskPath <- reactiveVal()
refreshMask <- reactiveVal(0)
collectCoords <- reactiveVal(0)
stopCollection <- reactiveVal(0)
coords <- NULL


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
    if (!isImage(theMask) & isImage(theImage))
      theMask <<- ones(nrow(theImage), ncol(theImage), 3)

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

      sc <- max(dim(toDisplay) / 720)
      r <- 0.01 * min(nrow(toDisplay), ncol(toDisplay))

      setTo(toDisplay, morph(canny(theMask, 0, 0), "dilate",
                             k_shape = "ellipse",
                             k_height = max(1, 0.75 * sc),
                             k_width = max(1, 0.75 * sc)),
            "white", target = "self")

      ch1 <- extractChannel(theMask, 1)
      h <- imhist(ch1, nbins = 257, range = c(0, 256))
      valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]

      com <- lapply(valid, function(val) {
        cc <- connectedComponents(ch1 == val, stats = FALSE)
        lapply(1:cc$n, function(lab) {
          dt <- distanceTransform(border(cc$labels == lab, 1))
          apply(findNonZero(dt == max(dt)[1]), 2, mean) - 1
        })
      })

      lab <- lapply(com, function(l) {
        lapply(l, function(loc) {
          pget(theMask, loc[1], loc[2])[1]
        })
      })

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

      if (collectCoords() == 1) {
        if (nrow(coords) > 0) {
          drawPolyline(toDisplay, coords, closed = TRUE, color = "white", thickness = max(1, 1.5 * sc))
        }
      }

      if (collectCoords() > 0) {
        drawCircle(toDisplay, x = coords[, 1], y = coords[, 2],
                   radius = r * 1.5, thickness = -1, color = "white")
        drawCircle(toDisplay, x = coords[, 1], y = coords[, 2],
                   radius = r, thickness = -1, color = "red")
      }

      suppressMessages(
        write.Image(resize(toDisplay,
                           fx = input$videoQuality_x,
                           fy = input$videoQuality_x,
                           interpolation = "area"),
                    paste0(tmpDir, "/display.jpg"), TRUE))
    } else {
      suppressMessages(
        write.Image(zeros(1080, 1920, 3),
                    paste0(tmpDir, "/display.jpg"), TRUE))
    }

    printDisplay(printDisplay() + 1)
  }
})

observeEvent(input$polyButton_x, {
  if (isImage(theMask)) {
    showNotification("Click to draw the polygonal ROI. Double-click to stop.",
                     id = "mask_notif", duration = NULL,
                     type = "message")

    toggleAll("OFF")
    collectCoords(1)
  }
})

observeEvent(input$ellButton_x, {
  if (isImage(theMask)) {
    showNotification("Click to select 5 points along the periphery of the
                     ellipse/circle ROI. Double-click to cancel.",
                     id = "mask_notif", duration = NULL,
                     type = "message")
    toggleAll("OFF")
    collectCoords(2)
  }
})

observeEvent(input$plot_click, {
  if (collectCoords() > 0) {
    clck <- input$plot_click$coords_img
    clck$y <- -clck$y + nrow(theMask) + 1
    coords <<- rbind(coords, unlist(clck))

    if (collectCoords() == 2 & nrow(coords) >= 5) {
      stopCollection(stopCollection() + 1)
    }

    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$plot_dblclick, {
  if (collectCoords() > 0) {
    stopCollection(stopCollection() + 1)
  }
})

observeEvent(stopCollection(), {
  if (collectCoords() > 0) {
    sc <- max(dim(theMask) / 720)

    if (collectCoords() == 1) {
      if (nrow(coords) > 2) {
        polyMask <- zeros(nrow(theMask), ncol(theMask), 1)
        fillPoly(polyMask, coords, color = "white")

        if (input$incButton_x == "Including") {
          setTo(theMask, polyMask, gray(input$roi_x / 255), target = "self")
        } else if (input$incButton_x == "Excluding") {
          setTo(theMask, polyMask, "black", target = "self")
        }
      }
    } else {
      if (nrow(coords) == 5) {
        ell <- optimEllipse(coords[, 1], coords[, 2])
        ellMask <- zeros(nrow(theMask), ncol(theMask), 1)
        drawEllipse(ellMask, ell[1], ell[2], ell[3] / 2, ell[4] / 2, ell[5],
                    color = "white", thickness = -1)

        if (input$incButton_x == "Including") {
          setTo(theMask, ellMask, gray(input$roi_x / 255), target = "self")
        } else if (input$incButton_x == "Excluding") {
          setTo(theMask, ellMask, "black", target = "self")
        }
      }
    }

    removeNotification(id = "mask_notif")
    toggleAll("ON")
    collectCoords(0)
    coords <<- NULL
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
