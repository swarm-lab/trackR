theActive <- reactiveVal("video")
theBookmarks <- reactiveValues()
bookmarkExclude <- c("saveSettings", "loadSettings")

# Video slider
output$videoSlider <- renderUI({
  if (isVideo(theVideo()) & !is.null(input$rangePos)) {
    sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                max = diff(input$rangePos) + 1, step = 1)
  } else {
    sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                max = 1, step = 1)
  }
})

# Save settings
shinyFileSave(input, "saveSettings", roots = getVolumes())

observeEvent(input$saveSettings, {
  if (length(input$saveSettings) > 1)
    session$doBookmark()
})

onBookmark(function(state) {
  state$values$bookmarks <- reactiveValuesToList(theBookmarks)
})

onBookmarked(function(url) {
  state <- sub(".*(\\?_inputs_)", "", url)
  settings_path <- parseSavePath(roots = getVolumes(), input$saveSettings)
  saveRDS(state, settings_path$datapath)
})

# Load settings
shinyFileChoose(input, "loadSettings", roots = getVolumes())

observeEvent(input$loadSettings, {
  settings_path <- parseFilePaths(roots = getVolumes(), input$loadSettings)
  if (nrow(settings_path) > 0) {
    state <- readRDS(settings_path$datapath)
    url <- paste0("http://", session$clientData$url_hostname, ":",
                  session$clientData$url_port, "/?_inputs_", state)
    js$replace(url);
  }
})

onRestore(function(state) {
  for (i in names(state$values$bookmarks)) {
    theBookmarks[[i]] <- state$values$bookmarks[[i]]
  }
})
