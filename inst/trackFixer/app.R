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

    if(e.which == 81) {
      e.preventDefault();
      $('#reassignTrack')[0].click();
    };

    if(e.which == 87) {
      e.preventDefault();
      $('#removeTrack')[0].click();
    };

    if(e.which == 69) {
      e.preventDefault();
      $('#swapTrack')[0].click();
    };

    if(e.which == 82) {
      e.preventDefault();
      $('#revertChanges')[0].click();
    };

    if(e.which == 83) {
      e.preventDefault();
      $('#saveChanges')[0].click();
    };
  });
}"


#####
# UI
#####
ui <- function(request) {
  shinyUI(fluidPage(
    id = "ui",

    titlePanel("trackFixer"),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    useShinyjs(),
    extendShinyjs(text = jscode, functions = "onKeypress"),

    fluidRow(
      column(
        12,

        uiOutput("reassignModal"),
        uiOutput("removeModal"),
        uiOutput("swapModal"),

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

            tags$table(style = "width: 100%; margin-bottom: 10px;",
                       tags$tr(
                         actionButton("reassignTrack", "Reassign [q]", class = "halfWidth"),
                         actionButton("removeTrack", "Remove [w]", class = "halfWidth")
                       ),
                       tags$tr(
                         actionButton("swapTrack", "Swap IDs [e]", class = "halfWidth"),
                         actionButton("revertChanges", "Undo [r]", class = "halfWidth")
                       ),
                       tags$tr(
                         actionButton("saveChanges", "Save changes [s]", class = "fullWidth")
                       )
            )
          ),

          bsCollapsePanel(
            title = NULL,
            value = "statsPanel",
            tableOutput("trackStats"),
            tags$b(textOutput("filterTitle")),
            htmlOutput("filterSlider")
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
        }
      }
    }
  })

  output$videoStatus <- renderUI({
    if (!isVideo(theVideo())) {
      p("Video missing (and required).", class = "bad")
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
        if (any(!(c("ignore", "track_fixed") %in% names(toCheck)))) {
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
  # Display stats
  #####
  output$trackStats <- renderTable({
    if (is.data.frame(theTracks())) {
      tab <- table(theTracks()$track_fixed[!theTracks()$ignore])
      data.frame("Number of tracks" = length(tab),
                 "Shortest" = min(tab),
                 "Longest" = max(tab),
                 "Median" = median(tab),
                 check.names = FALSE)
    } else {
      data.frame("Number of tracks" = NA,
                 "Shortest" = NA,
                 "Longest" = NA,
                 "Median" = NA,
                 check.names = FALSE)
    }
  }, striped = TRUE, width = "100%", align = "c")

  #####
  # Filter by length
  #####
  output$filterSlider <- renderUI({
    if (tracksLoaded() > 0) {
      isolate({
        tab <- table(theTracks()$track_fixed)
      })
      sliderInput("lengthFilter", NULL, width = "100%",
                  value = 1, min = 1, max = max(tab), step = 1)
    } else {
      sliderInput("lengthFilter", NULL, width = "100%",
                  value = 0, min = 0, max = 0, step = 1)
    }
  })

  output$filterTitle <- renderText({
    paste0("Filter tracks shorter than ", input$lengthFilter, " frames")
  })

  observeEvent(input$lengthFilter, {
    if (is.data.frame(theTracks())) {
      isolate({
        tab <- table(theTracks()$track_fixed[theTracks()$ignore == FALSE])
        toIgnore <- as.numeric(names(tab)[tab < input$lengthFilter])
        tmp <- theTracks()
        tmp$ignore[tmp$track_fixed %in% toIgnore] <- TRUE
        theTracks(tmp)
      })
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
        d <- input$videoSize * max(dim(theImage())) / 720
        drawCircle(theImage(), tmp[idx, ]$x[m], tmp[idx, ]$y[m], 10 * d, "grey50", -1)
        drawText(theImage(), j, tmp[idx, ]$x[m] - (if (j < 10) 4 * d else 8 * d),
                 tmp[idx, ]$y[m] - 4 * d, font_scale = 0.4 * d, thickness = 1.5 * d, color = "white")
      }

      display(theImage(), "trackFixer", 1,
              round(nrow(theImage()) * input$videoSize),
              round(ncol(theImage()) * input$videoSize))
    }
  })

  #####
  # Fix tracks
  #####
  changes <- list()

  # Reassign
  reassign <- reactiveVal(0)

  output$reassignModal <- renderUI({
    if (is.data.frame(theTracks())) {
      idx <- !theTracks()$ignore & theTracks()$frame == input$videoPos
      ids <- c("", sort(unique(theTracks()$track_fixed[idx])))
    } else {
      ids <- ""
    }

    bsModal(
      id = "reassignTrackDialog",
      title = "Reassign track",
      trigger = "reassignTrack",
      selectInput("currentID", "Select track to reassign", ids, width = "100%"),
      numericInput("newID", "Type ID to reassign it to", NA, 0, Inf, width = "100%"),
      tags$hr(),
      tags$table(style = "width: 100%;",
                 tags$tr(
                   actionButton("cancelReassign", "Cancel", class = "halfWidth"),
                   actionButton("okReassign", "Done", class = "halfWidth")
                 )
      )
    )
  })

  observeEvent(input$cancelReassign, {
    toggleModal(session, "reassignTrackDialog", "close")
  })

  observeEvent(input$okReassign, {
    toggleModal(session, "reassignTrackDialog", "close")
    reassign(reassign() + 1)
  })

  observe({
    if (reassign() > 0) {
      if (!input$reassignTrackDialog) {
        isolate({
          if (is.data.frame(theTracks()) & !is.na(input$newID)) {
            tmp <- theTracks()
            idx <- tmp$track_fixed == input$currentID
            tmp$track_fixed[idx] <- as.numeric(input$newID)
            theTracks(tmp)

            changes[[length(changes) + 1]] <<- list(frame = input$videoPos,
                                                    type = "reassign",
                                                    idx = which(idx),
                                                    revert = as.numeric(input$currentID))
          }
        })
      }
    }
  })

  # Remove
  remove <- reactiveVal(0)

  output$removeModal <- renderUI({
    if (is.data.frame(theTracks())) {
      idx <- !theTracks()$ignore & theTracks()$frame > (input$videoPos - 1 * fps(theVideo())) &
        theTracks()$frame <= input$videoPos
      ids <- c("", sort(unique(theTracks()$track_fixed[idx])))
    } else {
      ids <- ""
    }

    bsModal(
      id = "removeTrackDialog",
      title = "Remove track",
      trigger = "removeTrack",
      selectInput("removeID", "Select track to remove", ids, width = "100%", selected = NA),
      tags$hr(),
      tags$table(style = "width: 100%;",
                 tags$tr(
                   actionButton("cancelRemove", "Cancel", class = "halfWidth"),
                   actionButton("okRemove", "Done", class = "halfWidth")
                 )
      )
    )
  })

  observeEvent(input$cancelRemove, {
    toggleModal(session, "removeTrackDialog", "close")
  })

  observeEvent(input$okRemove, {
    toggleModal(session, "removeTrackDialog", "close")
    remove(remove() + 1)
  })

  observe({
    if (remove() > 0) {
      if (!input$removeTrackDialog) {
        isolate({
          if (is.data.frame(theTracks()) & !is.na(input$removeID)) {
            tmp <- theTracks()
            idx <- tmp$track_fixed == input$removeID
            tmp$ignore[idx] <- TRUE
            theTracks(tmp)

            changes[[length(changes) + 1]] <<- list(frame = input$videoPos,
                                                    type = "remove",
                                                    idx = which(idx),
                                                    revert = FALSE)
          }
        })
      }
    }
  })

  # Swap
  swap <- reactiveVal(0)

  output$swapModal <- renderUI({
    if (is.data.frame(theTracks())) {
      idx <- !theTracks()$ignore & theTracks()$frame == input$videoPos
      ids <- c("", sort(unique(theTracks()$track_fixed[idx])))
    } else {
      ids <- ""
    }

    bsModal(
      id = "swapTrackDialog",
      title = "Swap track IDs",
      trigger = "swapTrack",

      tags$table(
        style = "width: 100%;",
        tags$tr(
          tags$td(selectInput("swapID1", "Select first track", ids, selected = NA, width = "100%"),
                  class = "halfWidth"),
          tags$td(selectInput("swapID2", "Select second track", ids, selected = NA, width = "100%"),
                  class = "halfWidth")
        )
      ),
      tags$p("Note: the track IDs will be swapped from this point on.", class = "good"),
      tags$hr(),
      tags$table(
        style = "width: 100%;",
        tags$tr(
          actionButton("cancelSwap", "Cancel", class = "halfWidth"),
          actionButton("okSwap", "Done", class = "halfWidth")
        )
      )
    )
  })

  observeEvent(input$cancelSwap, {
    toggleModal(session, "swapTrackDialog", "close")
  })

  observeEvent(input$okSwap, {
    toggleModal(session, "swapTrackDialog", "close")
    swap(swap() + 1)
  })

  observe({
    if (swap() > 0) {
      if (!input$swapTrackDialog) {
        isolate({
          if (is.data.frame(theTracks()) & !is.na(input$swapID1) & !is.na(input$swapID2)) {
            tmp <- theTracks()
            idx1 <- tmp$track_fixed == input$swapID1 & tmp$frame >= input$videoPos
            idx2 <- tmp$track_fixed == input$swapID2 & tmp$frame >= input$videoPos

            tmp$track_fixed[idx1] <- as.numeric(input$swapID2)
            tmp$track_fixed[idx2] <- as.numeric(input$swapID1)

            theTracks(tmp)

            changes[[length(changes) + 1]] <<- list(frame = input$videoPos,
                                                    type = "swap",
                                                    idx1 = which(idx1),
                                                    idx2 = which(idx2),
                                                    revert1 = as.numeric(input$swapID1),
                                                    revert2 = as.numeric(input$swapID2))
          }
        })
      }
    }
  })

  # Save
  observeEvent(input$saveChanges, {
    if (is.data.frame(theTracks())) {
      path <- parseFilePaths(roots = getVolumes(), input$trackFile)
      write_csv(theTracks(), paste0(sub(".csv", "", path$datapath), "_fixed.csv"))
      showNotification(paste0("Changes saved at ",
                              paste0(sub(".csv", "", path$datapath), "_fixed.csv")),
                       id = "save", duration = 2)
    }
  })

  # Undo
  observeEvent(input$revertChanges, {
    isolate({
      if (length(changes) > 0) {
        l <- length(changes)
        tmp <- theTracks()

        if (changes[[l]]$type == "reassign") {
          tmp$track_fixed[changes[[l]]$idx] <- changes[[l]]$revert
        } else if (changes[[l]]$type == "remove") {
          tmp$ignore[changes[[l]]$idx] <- changes[[l]]$revert
        } else if (changes[[l]]$type == "swap") {
          tmp$track_fixed[changes[[l]]$idx1] <- changes[[l]]$revert1
          tmp$track_fixed[changes[[l]]$idx2] <- changes[[l]]$revert2
        }

        updateSliderInput(session, "videoPos", value = changes[[l]]$frame)
        changes[[l]] <<- NULL
        theTracks(tmp)
      }
    })
  })

  #####
  # Clean up
  #####
  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
