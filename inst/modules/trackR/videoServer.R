# Variables and reactives
volumes <- c(Home = fs::path_home(), getVolumes()())
theVideo <- NULL
theImage <- NULL
rangeMem <- c(NA, NA)
frameMem <- NA

theVideoPath <- reactiveVal()
theFrame <- reactiveVal()
defaultRoot <- reactiveVal()
defaultPath <- reactiveVal("")
refreshVideo <- reactiveVal(0)
refreshDisplay <- reactiveVal(0)


# Outputs
output$videoStatus <- renderUI({
  if (refreshDisplay() > -1 & !isVideoStack(theVideo) &
      length(input$sortedVideos) == 0) {
    disable(selector = "[data-value='2']")
    disable(selector = "[data-value='3']")
    disable(selector = "[data-value='4']")
    disable(selector = "[data-value='5']")
    disable(selector = "[data-value='6']")
    p("Video missing (and required).", class = "bad")
  } else if (!isVideoStack(theVideo)) {
    disable(selector = "[data-value='2']")
    disable(selector = "[data-value='3']")
    disable(selector = "[data-value='4']")
    disable(selector = "[data-value='5']")
    disable(selector = "[data-value='6']")
    p("Incompatible videos.", class = "bad")
  } else {
    enable(selector = "[data-value='2']")
  }
})

output$videoList <- renderUI({
  if (!is.null(theVideoPath())) {
    tags$div(
      class = "panel panel-default",

      tags$div(
        class = "panel-heading",
        icon("video"),
        "Selected videos (drag to reorder)"
      ),

      rank_list(
        text = NULL,
        labels = theVideoPath()$name,
        input_id = "sortedVideos",
        options = sortable_options(
          group = list(group = "group")
        )
      ),

      tags$div(
        class = "panel-heading",
        icon("trash"),
        "Remove video from list (drag & drop below)"
      ),
      tags$div(
        class = "panel-body",
        style = "background: repeating-linear-gradient(
                -45deg,
                #f5f5f5,
                #f5f5f5 10px,
                #999999 10px,
                #999999 20px
              );",
        id = "bin"
      ),
      sortable_js(
        "bin",
        options = sortable_options(
          group = list(
            group = "group",
            put = TRUE,
            pull = TRUE
          ),
          onAdd = htmlwidgets::JS("function (evt) { this.el.removeChild(evt.item); }")
        )
      )
    )
  }
})

output$rangeSlider <- renderUI({
  if (refreshVideo() > 0 & isVideoStack(theVideo)) {
    sliderInput("rangePos_x", "Video range", width = "100%", min = 1,
                max = nframes(theVideo),
                value = c(1, nframes(theVideo)), step = 1)
  }
})

output$displaySlider <- renderUI({
  if (refreshVideo() > 0 & isVideoStack(theVideo)) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$qualitySlider <- renderUI({
  if (refreshVideo() > 0 & isVideoStack(theVideo)) {
    sliderInput("videoQuality_x", "Video quality", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$videoSlider <- renderUI({
  if (refreshVideo() > 0 & !is.null(input$rangePos_x) & isVideoStack(theVideo)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      sliderInput("videoPos_x", "Frame", width = "100%", step = 1,
                  value = input$rangePos_x[2],
                  min = input$rangePos_x[1],
                  max = input$rangePos_x[2])
    } else {
      sliderInput("videoPos_x", "Frame", width = "100%", step = 1,
                  value = input$rangePos_x[1],
                  min = input$rangePos_x[1],
                  max = input$rangePos_x[2])
    }
  }
})


# Events
observeEvent(input$main, {
  if (input$main == "1") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

shinyFileChoose(input, "videoFile_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$videoFile_x, {
  path <- parseFilePaths(volumes, input$videoFile_x)
  if (nrow(path) > 0) {
    if (is.data.frame(theVideoPath())) {
      ord <- match(input$sortedVideos, theVideoPath()$name)
      tmp <- rbind(theVideoPath()[ord, ], path)
      theVideoPath(tmp[!duplicated(tmp), ])
    } else {
      theVideoPath(path)
    }
  }
})

observeEvent(input$sortedVideos, {
  l <- nrow(theVideoPath())

  ix <- which.max(
    sapply(
      stringr::str_locate_all(theVideoPath()$datapath[l], volumes),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      })
  )
  volume <- volumes[ix]

  if (length(volume) > 0) {
    dir <- dirname(theVideoPath()$datapath[l])
    defaultRoot(names(volumes)[ix])
    defaultPath(gsub(volume, "", dir))
  }
})

observeEvent(input$sortedVideos, {
  if (isVideoStack(theVideo)) {
    lapply(theVideo, function(x) x$release())

    if (!all(sapply(theVideo, function(x) x$isOpened()))) {
      theVideo <<- NULL
    } else {
      cat("An error occured while trying to release the video stack.\n")
    }
  }

  ord <- match(input$sortedVideos, theVideoPath()$name)
  toCheck <- tryCatch(videoStack(unlist(theVideoPath()$datapath[ord])),
                      error = function(e) NA)

  if (isVideoStack(toCheck)) {
    if (nframes(toCheck) > 0) {
      theVideo <<- toCheck
      theImage <<- readFrame(theVideo, 1)

      if (isImage(theBackground)) {
        if (!all(dim(theBackground) == dim(theImage))) {
          theBackground <<- NULL
        }
      }

      if (isImage(theMask)) {
        if (!all(dim(theMask) == dim(theImage))) {
          theMask <<- NULL
        }
      }
    } else {
      theVideo <<- NULL
      theImage <<- NULL
    }
  } else {
    theImage <<- NULL
  }

  refreshVideo(refreshVideo() + 1)
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$videoSize_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(input$videoQuality_x, {
  refreshDisplay(refreshDisplay() + 1)
})

observeEvent(theFrame(), {
  if (!is.null(theFrame())) {
    readFrame(theVideo, theFrame(), theImage)
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(refreshDisplay(), {
  if (input$main == "1") {
    if (isImage(theImage)) {
      toDisplay <- cloneImage(theImage)
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

observeEvent(input$videoPos_x, {
  theFrame(input$videoPos_x)
  frameMem <<- input$videoPos_x

  if (input$main == "1") {
    if (!is.null(input$videoPos2_x))
      updateSliderInput(session, "videoPos2_x", value = input$videoPos_x)

    if (!is.null(input$videoPos3_x))
      updateSliderInput(session, "videoPos3_x", value = input$videoPos_x)
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "refreshVideo", "refreshDisplay",
                     "videoFile_x", "videoFile_x-modal"))

onBookmark(function(state) {
  state$values$theVideoPath <- theVideoPath()
})

onRestore(function(state) {
  theVideoPath(state$values$theVideoPath[[1]])
})
