theBackgroundPath <- reactiveVal()
theBackground <- reactiveVal()
refreshBackground <- reactiveVal(0)

# Load background
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
    toCheck <- tryCatch(Rvision::image(theBackgroundPath()),
                        error = function(e) NA)

    if (Rvision::isImage(toCheck)) {
      theBackground(Rvision::changeColorSpace(toCheck, "BGR"))
      ix <- sapply(volumes, grepl, x = theBackgroundPath())
      volume <- volumes[ix]
      dir <- dirname(theBackgroundPath())
      defaultRoot(names(volumes)[ix])
      defaultPath(gsub(volume, "", dir))
    }
  }
})

# Compute background
observeEvent(input$computeBackground_x, {
  if (Rvision::isVideo(theVideo())) {
    toggleAll("OFF")
    theBackground(backgrounder(theVideo(), n = input$backroundImages_x,
                               method = input$backgroundType_x,
                               start = input$rangePos_x[1],
                               end = input$rangePos_x[2]))
    toggleAll("ON")
  }
})

output$backgroundStatus <- renderUI({
  if (!Rvision::isImage(theBackground())) {
    p("Background missing (and required).", class = "bad")
  }
})

# Display background
observe({
  if (input$main == "2") {
    if (Rvision::isImage(theBackground())) {
      if (is.null(input$videoSize_x)) {
        Rvision::display(theBackground(), "trackR", 25,
                         nrow(theBackground()),
                         ncol(theBackground()))
      } else {
        Rvision::display(
          theBackground(),
          "trackR", 25,
          nrow(theBackground()) * input$videoSize_x,
          ncol(theBackground()) * input$videoSize_x)
      }
    } else {
      if (is.null(input$videoSize_x)) {
        Rvision::display(Rvision::zeros(480, 640), "trackR", 25, 480, 640)
      } else {
        Rvision::display(Rvision::zeros(nrow(theImage()), ncol(theImage())),
                         "trackR", 25,
                         nrow(theImage()) * input$videoSize_x,
                         ncol(theImage()) * input$videoSize_x)
      }
    }
  }
})

# Remove ghosts
observeEvent(input$ghostButton_x, {
  if (Rvision::isImage(theBackground())) {
    toggleAll("OFF")

    showNotification("Use left click to draw the ROI. Use right click to close
                       it and return the result.", id = "bg_notif", duration = NULL,
                     type = "message")

    if (is.null(input$videoSize_x)) {
      suppressMessages(
        ROI <- Rvision::selectROI(theBackground(), "trackR", 1, TRUE)
      )
    } else {
      suppressMessages(
        ROI <- Rvision::selectROI(theBackground(), "trackR",
                                  input$videoSize_x, TRUE)
      )
    }

    removeNotification(id = "bg_notif")

    theBackground(Rvision::inpaint(theBackground(), ROI$mask, method = "Telea"))

    toggleAll("ON")
  }
})

# Save background
shinyFileSave(input, "saveBackground_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$saveBackground_x, {
  path <- parseSavePath(volumes, input$saveBackground_x)

  if (Rvision::isImage(theBackground()) & nrow(path) > 0) {
    Rvision::write.Image(theBackground(), path$datapath)
    theBackgroundPath(path$datapath)
  }
})

# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "backgroundFile_x",
                     "computeBackground_x", "ghostButton_x", "saveBackground_x"))

onBookmark(function(state) {
  state$values$theBackgroundPath <- theBackgroundPath()
})

onRestore(function(state) {
  theBackgroundPath(state$values$theBackgroundPath[[1]])
  refreshBackground(refreshBackground() + 1)
})
