# Variables and reactives
theBackground <- NULL
theBackgroundPath <- reactiveVal()
refreshBackground <- reactiveVal(0)


# Outputs
output$backgroundStatus <- renderUI({
  if (refreshDisplay() > -1 & !isImage(theBackground)) {
    p("Background missing (and required).", class = "bad")
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

      theBackground <<- cloneImage(toCheck)
      refreshDisplay(refreshDisplay() + 1)

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
    }
  }
})

observeEvent(input$computeBackground_x, {
  if (isVideo(theVideo)) {
    toggleAll("OFF")
    theBackground <<- backgrounder(theVideo, n = input$backroundImages_x,
                                   method = input$backgroundType_x,
                                   start = input$rangePos_x[1],
                                   end = input$rangePos_x[2])
    changeBitDepth(theBackground, "8U", target = "self")
    toggleAll("ON")
    refreshBackground(refreshBackground() + 1)
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "2") {
    if (isImage(theBackground)) {
      toDisplay <- theBackground
    } else if (isImage(theImage)) {
      toDisplay <- zeros(nrow(theImage), ncol(theImage), 3)
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

observeEvent(input$ghostButton_x, {
  if (isImage(theBackground)) {
    toggleAll("OFF")

    showNotification("Use left click to draw the ROI. Use right click to close
                       it and return the result.", id = "bg_notif", duration = NULL,
                     type = "message")

    if (is.null(input$videoSize_x)) {
      suppressMessages(
        ROI <- selectROI(theBackground, "trackR", 1, TRUE)
      )
    } else {
      suppressMessages(
        ROI <- selectROI(theBackground, "trackR", input$videoSize_x, TRUE)
      )
    }

    inpaint(theBackground, ROI$mask, method = "Telea", target = "self")

    removeNotification(id = "bg_notif")
    toggleAll("ON")
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
