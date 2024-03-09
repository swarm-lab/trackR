# Variables and reactives
theBackground <- NULL
theBackgroundPath <- reactiveVal()
refreshBackground <- reactiveVal(0)
collectGhost <- reactiveVal(0)
stopGhostCollection <- reactiveVal(0)
ghostCoords <- NULL


# Status
output$backgroundStatus <- renderUI({
  if (refreshDisplay() > -1 & !isImage(theBackground)) {
    toggleTabs(3:6, "OFF")
    p("Background missing (and required).", class = "bad")
  } else {
    toggleTabs(3, "ON")
  }
})


# Events
observeEvent(input$main, {
  if (input$main == "2") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFileChoose(input, "backgroundFile_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$backgroundFile_x, {
  path <- parseFilePaths(volumes, input$backgroundFile_x)
  if (nrow(path) > 0) {
    theBackgroundPath(path$datapath)
    refreshBackground(refreshBackground() + 1)
  }
})

observeEvent(refreshBackground(), {
  if (refreshBackground() > 0) {
    toCheck <- tryCatch(image(theBackgroundPath()), error = function(e) NA)

    if (isImage(toCheck)) {
      if (colorspace(toCheck) != "BGR")
        changeColorSpace(toCheck, "BGR", "self")

      if (!all(dim(toCheck) == dim(theImage))) {
        shinyalert("Error:",
                   "The video and background do not have the same dimensions.",
                   type = "error", animation = FALSE,
                   closeOnClickOutside = TRUE)
        theBackground <<- NULL
      } else {
        theBackground <<- cloneImage(toCheck)
      }

      ix <- which.max(
        sapply(
          stringr::str_locate_all(theBackgroundPath(), volumes),
          function(l) {
            if (nrow(l) > 0) {
              diff(l[1, ])
            } else {
              NA
            }
          })
      )

      volume <- volumes[ix]
      dir <- dirname(theBackgroundPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))

      refreshDisplay(refreshDisplay() + 1)
    }
  }
})

observeEvent(input$computeBackground_x, {
  if (isVideoStack(theVideo)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")
    theBackground <<- backgrounder(theVideo, n = input$backroundImages_x,
                                   method = input$backgroundType_x,
                                   start = input$rangePos_x[1],
                                   end = input$rangePos_x[2])
    changeBitDepth(theBackground, "8U", target = "self")
    toggleInputs("ON")
    toggleTabs(1:2, "ON")
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "2") {
    if (isImage(theBackground)) {
      toDisplay <- cloneImage(theBackground)
      sc <- max(dim(toDisplay) / 720)
      r <- 0.01 * min(nrow(toDisplay), ncol(toDisplay))

      if (collectGhost() > 0) {
        if (nrow(ghostCoords) > 1) {
          drawPolyline(toDisplay, ghostCoords, closed = TRUE, color = "white",
                       thickness = max(1, 1.5 * sc))
        }

        if (nrow(ghostCoords) > 0) {
          drawCircle(toDisplay, x = ghostCoords[, 1], y = ghostCoords[, 2],
                     radius = r * 1.5, thickness = -1, color = "white")
          drawCircle(toDisplay, x = ghostCoords[, 1], y = ghostCoords[, 2],
                     radius = r, thickness = -1, color = "red")
        }
      }

      suppressMessages(write.Image(toDisplay, paste0(tmpDir, "/display.jpg"), TRUE))
    } else if (isImage(theImage)) {
      suppressMessages(write.Image(zeros(nrow(theImage), ncol(theImage), 3),
                                   paste0(tmpDir, "/display.jpg"), TRUE))
    } else {
      suppressMessages(write.Image(zeros(1080, 1920, 3),
                                   paste0(tmpDir, "/display.jpg"), TRUE))
    }

    printDisplay(printDisplay() + 1)
  }
})

observeEvent(input$ghostButton_x, {
  if (isImage(theBackground)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")

    showNotification("Click to draw a polygon around the object to remove from
                     the image. Double-click to stop.", id = "ghost_notif",
                     duration = NULL, type = "message")

    collectGhost(1)
  }
})

observeEvent(input$plot_click, {
  if (collectGhost() > 0) {
    clck <- input$plot_click$coords_img
    clck$y <- -clck$y + nrow(theBackground) + 1
    ghostCoords <<- rbind(ghostCoords, unlist(clck))
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$plot_dblclick, {
  if (collectGhost() > 0) {
    stopGhostCollection(stopGhostCollection() + 1)
  }
})

observeEvent(stopGhostCollection(), {
  if (collectGhost() > 0) {

    if (nrow(ghostCoords) > 0) {
      roi <- zeros(nrow(theBackground), ncol(theBackground), 1)
      fillPoly(roi, ghostCoords, "white")
      inpaint(theBackground, roi, method = "Telea", target = "self")
    }

    removeNotification(id = "ghost_notif")
    toggleInputs("ON")
    toggleTabs(1:2, "ON")
    collectGhost(0)
    ghostCoords <<- NULL
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFileSave(input, "saveBackground_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$saveBackground_x, {
  path <- parseSavePath(volumes, input$saveBackground_x)

  if (isImage(theBackground) & nrow(path) > 0) {
    write.Image(theBackground, path$datapath, TRUE)
    theBackgroundPath(path$datapath)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "backgroundFile_x", "refreshBackground",
                     "computeBackground_x", "ghostButton_x", "saveBackground_x"))

onBookmark(function(state) {
  state$values$theBackgroundPath <- theBackgroundPath()
})

onRestore(function(state) {
  theBackgroundPath(state$values$theBackgroundPath[[1]])
  refreshBackground(refreshBackground() + 1)
})
