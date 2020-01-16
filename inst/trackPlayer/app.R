library(shiny)
library(shinyBS)
library(shinyFiles)
library(shinyjs)
library(Rvision)
library(readr)
library(trackR)

#####
# Javascript
#####
jscode <- "shinyjs.init = function() {
  $(document).keydown(function(e) {
    // alert('Key pressed: ' + e.which);

    if(e.which == 32) {
      e.preventDefault();
      $('#playPause')[0].click();
    };

    if(e.which == 37) {
      e.preventDefault();
      $('#minusFrame')[0].click();
    };

    if(e.which == 39) {
      e.preventDefault();
      $('#plusFrame')[0].click();
    };

    if(e.which == 40) {
      e.preventDefault();
      $('#minusSec')[0].click();
    };

    if(e.which == 38) {
      e.preventDefault();
      $('#plusSec')[0].click();
    };
  });
}"


#####
# UI
#####
ui <- function(request) {
  shinyUI(fluidPage(
    id = "ui",

    titlePanel("trackPlayer"),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    useShinyjs(),
    extendShinyjs(text = jscode, functions = "onKeypress"),

    fluidRow(
      column(
        12,

        bsCollapse(
          id = "panels",
          multiple = TRUE,
          open = c("loadingPanel", "controlPanel", "statsPanel"),
          bsCollapsePanel(
            title = NULL,
            value = "loadingPanel",
            htmlOutput("videoStatus"),
            htmlOutput("trackStatus"),

            tags$table(style = "width: 100%; margin-bottom: 10px;",
                       tags$tr(
                         shinyFilesButton("videoFile", "Load video", "Please select a video file",
                                          FALSE, class = "halfWidth"),
                         shinyFilesButton("trackFile", "Load tracks", "Please select a track file",
                                          FALSE, class = "halfWidth")
                       )
            ),

            sliderInput("videoSize", "Display size", width = "100%", value = 1,
                        min = 0.1, max = 1, step = 0.1)
          ),

          bsCollapsePanel(
            title = NULL,
            value = "controlPanel",
            sliderInput("rangePos", "Video range", width = "100%", value = c(0, 1),
                        min = 0, max = 1, step = 1, ticks = FALSE),

            tags$table(style = "width: 100%; margin-bottom: 10px;",
                       tags$tr(
                         actionButton("playPause", "Play/Pause [space]", width = "100%")
                       ),
                       tags$tr(
                         actionButton("minusSec", "<< [↓]", class = "quarterWidth"),
                         actionButton("minusFrame", "< [←]", class = "quarterWidth"),
                         actionButton("plusFrame", "[→] >", class = "quarterWidth"),
                         actionButton("plusSec", "[↑] >>", class = "quarterWidth")
                       )
            ),

            sliderInput("videoPos", NULL, width = "100%", value = 0, min = 0,
                        max = 0, step = 1, ticks = FALSE),

            tags$hr(),

            htmlOutput("exportButton")
          )
        )
      )
    )
  ))
}


#####
# SERVER
#####
server <- function(input, output, session) {
  newDisplay("trackFixer")

  cbPalette <- c("#FFBF80", "#FF8000", "#FFFF99", "#FFFF33", "#B2FF8C", "#33FF00",
                 "#A6EDFF", "#1AB2FF", "#CCBFFF", "#664CFF", "#FF99BF", "#E61A33")

  #####
  # Select video
  #####
  theVideo <- reactiveVal()
  startPos <- reactiveVal(0)
  endPos <- reactiveVal(1)

  shinyFileChoose(input, "videoFile", roots = getVolumes())

  observeEvent(input$videoFile, {
    path <- parseFilePaths(roots = getVolumes(), input$videoFile)
    if (nrow(path) > 0) {
      toCheck <- tryCatch(Rvision::video(path$datapath),
                          error = function(e) NA)

      if (isVideo(toCheck)) {
        if (!is.na(nframes(toCheck))) {
          theVideo(toCheck)
          updateSliderInput(session, "videoPos", value = 1, min = 1, max = nframes(toCheck))
          updateSliderInput(session, "rangePos", value = c(0, nframes(toCheck)),
                            min = 1, max = nframes(toCheck))
        }
      }
    }
  })

  output$videoStatus <- renderUI({
    if (!isVideo(theVideo())) {
      p("Video missing (and required).", class = "bad")
    }
  })

  observeEvent(input$rangePos, {
    if (input$rangePos[1] != startPos()) {
      startPos(input$rangePos[1])
    } else if (input$rangePos[2] != endPos()) {
      endPos(input$rangePos[2])
    }
  })

  observeEvent(startPos(), {
    if (input$videoPos < startPos()) {
      updateSliderInput(session, "videoPos", value = startPos(), min = startPos())
    } else {
      updateSliderInput(session, "videoPos", min = startPos())
    }
  })

  observeEvent(endPos(), {
    if (input$videoPos > endPos()) {
      updateSliderInput(session, "videoPos", value = endPos(), max = endPos())
    } else {
      updateSliderInput(session, "videoPos", max = endPos())
    }
  })

  #####
  # Select tracks
  #####
  theTracks <- reactiveVal()
  tracksLoaded <- reactiveVal(0)

  shinyFileChoose(input, "trackFile", roots = getVolumes())

  observeEvent(input$trackFile, {
    path <- parseFilePaths(roots = getVolumes(), input$trackFile)
    if (nrow(path) > 0) {
      toCheck <- tryCatch(readr::read_csv(path$datapath),
                          error = function(e) NA)

      if (all(c("x", "y", "frame", "track") %in% names(toCheck))) {
        if (is.null(toCheck$ignore)) {
          toCheck$ignore <- FALSE
          toCheck$track_fixed <- toCheck$track
        }

        theTracks(toCheck)
        tracksLoaded(tracksLoaded() + 1)
      }
    }
  })

  output$trackStatus <- renderUI({
    if (!is.data.frame(theTracks())) {
      p("Tracks missing (and required).", class = "bad")
    }
  })

  #####
  # Display tracks
  #####
  play <- reactiveVal(FALSE)
  theFrame <- reactiveVal(1)
  theImage <- reactiveVal()

  observeEvent(input$playPause, {
    if (!play() & isVideo(theVideo())) {
      play(TRUE)
    } else {
      play(FALSE)
    }
  })

  observeEvent(input$videoPos, {
    isolate({
      if (isVideo(theVideo())) {
        if (input$videoPos - theFrame() == 1) {
          theImage(readNext(theVideo()))
        } else {
          theImage(readFrame(theVideo(), input$videoPos))
        }

        theFrame(input$videoPos)
      }
    })
  })

  observeEvent(theTracks(), {
    isolate({
      if (isVideo(theVideo())) {
        if (input$videoPos - theFrame() == 1) {
          theImage(readNext(theVideo()))
        } else {
          theImage(readFrame(theVideo(), input$videoPos))
        }

        theFrame(input$videoPos)
      }
    })
  })

  observe({
    if (play() & isVideo(theVideo())) {
      updateSliderInput(session, "videoPos", value = input$videoPos + 1)
    }
  })

  observeEvent(input$minusFrame, {
    if (isVideo(theVideo()))
      updateSliderInput(session, "videoPos", value = input$videoPos - 1)
  })

  observeEvent(input$plusFrame, {
    if (isVideo(theVideo()))
      updateSliderInput(session, "videoPos", value = input$videoPos + 1)
  })

  observeEvent(input$minusSec, {
    if (isVideo(theVideo()))
      updateSliderInput(session, "videoPos", value = input$videoPos - fps(theVideo()))
  })

  observeEvent(input$plusSec, {
    if (isVideo(theVideo()))
      updateSliderInput(session, "videoPos", value = input$videoPos + fps(theVideo()))
  })

  observe({
    if (isImage(theImage()) & tracksLoaded() > 0) {
      idx <- !theTracks()$ignore &
        theTracks()$frame > (input$videoPos - 1 * fps(theVideo())) &
        theTracks()$frame <= input$videoPos
      tmp <- theTracks()[idx, ]

      for (j in sort(unique(tmp$track_fixed))) {
        idx <- tmp$track_fixed == j

        if (length(tmp[idx, ]$x) > 1) {
          drawLine(theImage(), tmp[idx, ]$x[1:(length(tmp[idx, ]$x) - 1)],
                   tmp[idx, ]$y[1:(length(tmp[idx, ]$x) - 1)],
                   tmp[idx, ]$x[2:length(tmp[idx, ]$x)],
                   tmp[idx, ]$y[2:length(tmp[idx, ]$x)],
                   cbPalette[(tmp[idx, ]$track_fixed %% 12) + 1],
                   round(ncol(theImage()) / 200))
        }
      }

      for (j in sort(unique(tmp$track_fixed))) {
        idx <- tmp$track_fixed == j
        m <- which.max(tmp[idx, ]$frame)
        d <- max(dim(theImage())) / 720
        drawCircle(theImage(), tmp[idx, ]$x[m], tmp[idx, ]$y[m], 20 * d, "grey50", -1)
        drawText(theImage(), j, tmp[idx, ]$x[m] - (if (j < 10) 8 * d else 16 * d),
                 tmp[idx, ]$y[m] - 8 * d, font_scale = 0.8 * d, thickness = 3 * d, color = "white")
      }

      display(theImage(), "trackFixer", 1,
              round(nrow(theImage()) * input$videoSize),
              round(ncol(theImage()) * input$videoSize))
    }
  })

  #####
  # Export video
  #####
  shinyFileSave(input, "exportFile", roots = getVolumes())

  output$exportButton <- renderUI({
    if (isVideo(theVideo()) & is.data.frame(theTracks())) {
      path <- parseFilePaths(roots = getVolumes(), input$videoFile)
      shinySaveButton("exportFile", "Export video with tracks", "Save file as...",
                      filetype = list(video = tools::file_ext(path$datapath)),
                      class = "halfWidth")
    } else {
      disabled(shinySaveButton("exportFile", "Export video with tracks", "Save file as...",
                               class = "halfWidth"))
    }
  })

  observeEvent(input$exportFile, {
    if (is.list(input$exportFile)) {
      path <- parseSavePath(roots = getVolumes(), input$exportFile)
      vw <- videoWriter(path$datapath,  codec(theVideo()), fps(theVideo()),
                        nrow(theVideo()), ncol(theVideo()))

      withProgress(message = "Preparing video",
                   min = input$rangePos[1],
                   max = input$rangePos[2], {
                     theFrame <- readFrame(theVideo(), input$rangePos[1])

                     for (i in input$rangePos[1]:input$rangePos[2]) {
                       idx <- !theTracks()$ignore & theTracks()$frame > (i - 1 * fps(theVideo())) &
                         theTracks()$frame <= i
                       tmp <- theTracks()[idx, ]

                       for (j in sort(unique(tmp$track_fixed))) {
                         idx <- tmp$track_fixed == j

                         if (length(tmp[idx, ]$x) > 1) {
                           drawLine(theFrame, tmp[idx, ]$x[1:(length(tmp[idx, ]$x) - 1)],
                                    tmp[idx, ]$y[1:(length(tmp[idx, ]$x) - 1)],
                                    tmp[idx, ]$x[2:length(tmp[idx, ]$x)],
                                    tmp[idx, ]$y[2:length(tmp[idx, ]$x)],
                                    cbPalette[(tmp[idx, ]$track_fixed %% 12) + 1],
                                    round(ncol(theFrame) / 200))
                         }
                       }

                       for (j in sort(unique(tmp$track_fixed))) {
                         idx <- tmp$track_fixed == j
                         m <- which.max(tmp[idx, ]$frame)
                         d <- max(dim(img)) / 720
                         drawCircle(theFrame, tmp[idx, ]$x[m], tmp[idx, ]$y[m], 20 * d, "grey50", -1)
                         drawText(theFrame, j, tmp[idx, ]$x[m] - (if (j < 10) 8 * d else 16 * d),
                                  tmp[idx, ]$y[m] - 8 * d, font_scale = 0.8 * d, thickness = 3 * d, color = "white")
                       }

                       writeFrame(vw, theFrame)

                       if (i != input$rangePos[2]) {
                         theFrame <- readNext(theVideo())
                         setProgress(value = i + 1)
                       }
                     }

                     release(vw)
                   })
    }
  })

  #####
  # Clean up
  #####
  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
