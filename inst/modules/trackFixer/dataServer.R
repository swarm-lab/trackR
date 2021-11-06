volumes <- c(Home = fs::path_home(), getVolumes()())
colNames <- c("x", "y", "n", "frame", "track", "width", "height", "angle")

theVideoPath <- reactiveVal()
theVideo <- reactiveVal()
theTracksPath <- reactiveVal()
theTracks <- reactiveVal()
theImage <- reactiveVal()
defaultRoot <- reactiveVal(NULL)
defaultPath <- reactiveVal("")

# Select video
shinyFileChoose(input, "videoFile_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$videoFile_x, {
  path <- parseFilePaths(volumes, input$videoFile_x)
  if (nrow(path) > 0) {
    theVideoPath(path$datapath)
  }
})

observeEvent(theVideoPath(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(theVideoPath(), volumes),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      })
  )
  volume <- volumes[ix]
  dir <- dirname(theVideoPath())
  defaultRoot(names(volumes)[ix])
  defaultPath(gsub(volume, "", dir))
})

observeEvent(theVideoPath(), {
  toCheck <- tryCatch(video(theVideoPath()),
                      error = function(e) NA)

  if (isVideo(toCheck)) {
    if (!is.na(nframes(toCheck))) {
      theVideo(toCheck)
    }
  }
})

output$videoStatus <- renderUI({
  if (!isVideo(theVideo())) {
    p("Video missing (and required).", class = "bad")
  }
})

# Select track file
shinyFileChoose(input, "trackFile_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$trackFile_x, {
  path <- parseFilePaths(roots = volumes, input$trackFile_x)
  if (nrow(path) > 0) {
    toCheck <- tryCatch(data.table::fread(path$datapath),
                        error = function(e) NA)

    if (all(colNames %in% names(toCheck))) {
      if (any(!(c("ignore", "track_fixed") %in% names(toCheck)))) {
        toCheck[, c("ignore", "track_fixed") := list(FALSE, track)]
      }
      theTracks(toCheck)
      theTracksPath(path$datapath)
    }
  }
})

observeEvent(theTracksPath(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(theTracksPath(), volumes),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      })
  )
  volume <- volumes[ix]
  dir <- dirname(theTracksPath())
  defaultRoot(names(volumes)[ix])
  defaultPath(gsub(volume, "", dir))
})

output$trackStatus <- renderUI({
  if (!is.data.frame(theTracks())) {
    p("Tracks missing (and required).", class = "bad")
  }
})
