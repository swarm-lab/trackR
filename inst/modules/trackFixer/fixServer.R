####################
# GUI
####################

# Enable panel
observe({
  if (isVideo(theVideo()) & is.data.frame(theTracks())) {
    enable(selector = "a[data-value=2]")
    updateVerticalTabsetPanel(session, "main", selected = "2")
  }
})

# Status
output$videoStatus2 <- renderUI({
  if (!isVideo(theVideo())) {
    p("Video missing (and required).", class = "bad")
  }
})

output$trackStatus2 <- renderUI({
  if (!is.data.frame(theTracks())) {
    p("Tracks missing (and required).", class = "bad")
  }
})

# Display slider
output$displaySlider <- renderUI({
  if (isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

# Video slider
output$videoSlider <- renderUI({
  if (isVideo(theVideo())) {
    if (is.data.frame(theTracks())) {
      sliderInput("videoPos_x", NULL, width = "100%",
                  value = min(theTracks()[, frame]), min = 1,
                  max = nframes(theVideo()), step = 1)
    } else {
      sliderInput("videoPos_x", NULL, width = "100%", value = 1, min = 1,
                  max = nframes(theVideo()), step = 1)
    }
  }
})

# Controls
play <- reactiveVal(FALSE)

observeEvent(input$playPause_x, {
  if (!play() & isVideo(theVideo())) {
    play(TRUE)
  } else {
    play(FALSE)
  }
})

observeEvent(input$videoSize_x, {
  theImage(readFrame(theVideo(), input$videoPos_x))
})

observeEvent(input$videoPos_x, {
  if (input$videoPos_x - theVideo()$frame() == 1) {
    theImage(readNext(theVideo()))
  } else {
    theImage(readFrame(theVideo(), input$videoPos_x))
  }
})

observe({
  if (play()) {
    updateSliderInput(session, "videoPos_x", value = input$videoPos_x + 1)
  }
})

observeEvent(input$minusFrame_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x - 1)
})

observeEvent(input$plusFrame_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x + 1)
})

observeEvent(input$minusSec_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x - theVideo()$fps())
})

observeEvent(input$plusSec_x, {
  updateSliderInput(session, "videoPos_x", value = input$videoPos_x + theVideo()$fps())
})

# Display video
refreshDisplay <- reactiveVal(0)

observe({
  if (isImage(theImage()) & is.data.frame(theTracks())) {
    isolate( refreshDisplay(refreshDisplay() + 1) )
  }
})

observeEvent(refreshDisplay(), {
  if (refreshDisplay() > 0) {
    tmp_rect <- theTracks()[ignore == FALSE & frame == input$videoPos_x, ]
    tmp_tracks <- theTracks()[ignore == FALSE &
                                frame >= (input$videoPos_x - 1 * theVideo()$fps()) &
                                frame <= input$videoPos_x, ]

    if (nrow(tmp_tracks) > 0) {
      sc <- max(dim(theImage()) / 720)
      overlay1 <- cloneImage(theImage())
      overlay2 <- cloneImage(theImage())

      if (nrow(tmp_rect) > 0) {
        drawRotatedRectangle(overlay1, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = 1.5 * sc)
        drawRotatedRectangle(overlay2, tmp_rect$x, tmp_rect$y,
                                      tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                      color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                      thickness = -1)
      }

      tmp_tracks[, drawPolyline(overlay2, cbind(x, y), FALSE,
                                         color = cbPalette[(track_fixed[1] %% 12) + 1],
                                         thickness = 3 * sc),
                 by = track_fixed]

      addWeighted(overlay1, overlay2, c(0.5, 0.5), target = "self")

      if (nrow(tmp_rect) > 0) {
        drawText(overlay1, tmp_rect$track_fixed,
                          tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                          tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                          color = "white")
      }

      display(overlay1, "trackFixer", 5,
                       nrow(overlay1) * input$videoSize_x,
                       ncol(overlay1) * input$videoSize_x)
    } else {
      display(theImage(), "trackFixer", 5,
                       nrow(theImage()) * input$videoSize_x,
                       ncol(theImage()) * input$videoSize_x)
    }
  } else {
    display(zeros(480, 640), "trackFixer", 5, 480, 640)
  }
})


####################
# Fix tracks
####################
changes <- list()

# Reassign
observeEvent(input$reassignTrack_x, {
  ids <- c("", theTracks()[ignore == FALSE & frame == input$videoPos_x]$track_fixed)

  showModal(
    modalDialog(
      title = "Reassign track",
      easyClose = TRUE,

      selectInput("currentID", "Select track to reassign", ids, width = "100%"),
      numericInput("newID", "Type ID to reassign it to", NA, 0, Inf, width = "100%"),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okReassign", "Reassign")
      )
    )
  )
})

observeEvent(input$okReassign, {
  removeModal(session)

  old_id <- as.numeric(input$currentID)
  new_id <- input$newID

  if (!is.na(old_id) & !is.na(new_id)) {
    idx <- theTracks()[, track_fixed] == old_id
    theTracks()[idx, track_fixed := new_id]
    changes[[length(changes) + 1]] <<- list(frame = input$videoPos_x,
                                            type = "reassign",
                                            idx = which(idx),
                                            revert = as.numeric(old_id))
    refreshStats(refreshStats() + 1)
    refreshDisplay(refreshDisplay() + 1)
  }
})

# Remove
observeEvent(input$removeTrack_x, {
  ids <- c("", theTracks()[ignore == FALSE & frame == input$videoPos_x]$track_fixed)

  showModal(
    modalDialog(
      title = "Remove track",
      easyClose = TRUE,

      selectInput("removeID", "Select track to remove", ids, width = "100%", selected = NA),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okRemove", "Remove")
      )
    )
  )
})

observeEvent(input$okRemove, {
  removeModal(session)

  rm_id <- as.numeric(input$removeID)

  if (!is.na(rm_id)) {
    idx <- theTracks()[, track_fixed] == rm_id & theTracks()[, frame] >= input$videoPos_x
    theTracks()[idx, ignore := TRUE]
    changes[[length(changes) + 1]] <<- list(frame = input$videoPos_x,
                                            type = "remove",
                                            idx = which(idx),
                                            revert = FALSE)
    refreshStats(refreshStats() + 1)
    refreshDisplay(refreshDisplay() + 1)
  }
})

# Swap
observeEvent(input$swapTrack_x, {
  ids <- c("", theTracks()[ignore == FALSE & frame == input$videoPos_x]$track_fixed)

  showModal(
    modalDialog(
      title = "Swap tracks",
      easyClose = TRUE,

      tags$table(
        style = "width: 100%;",
        tags$tr(
          tags$td(selectInput("swapID1", "Select first track", ids,
                              selected = NA, width = "100%"),
                  class = "halfWidth"),
          tags$td(selectInput("swapID2", "Select second track", ids,
                              selected = NA, width = "100%"),
                  class = "halfWidth")
        )
      ),
      tags$p("Note: the track IDs will be swapped from this point on.", class = "good"),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okSwap", "Swap")
      )
    )
  )
})

observeEvent(input$okSwap, {
  removeModal(session)

  id1 <- as.numeric(input$swapID1)
  id2 <- as.numeric(input$swapID2)

  if (!is.na(id1) & !is.na(id2)) {
    idx1 <- theTracks()[, track_fixed] == id1 & theTracks()[, frame] >= input$videoPos_x
    idx2 <- theTracks()[, track_fixed] == id2 & theTracks()[, frame] >= input$videoPos_x
    theTracks()[idx1, track_fixed := id2]
    theTracks()[idx2, track_fixed := id1]
    changes[[length(changes) + 1]] <<- list(frame = input$videoPos_x,
                                            type = "swap",
                                            idx1 = which(idx1),
                                            idx2 = which(idx2),
                                            revert1 = id1,
                                            revert2 = id2)
    refreshStats(refreshStats() + 1)
    refreshDisplay(refreshDisplay() + 1)
  }
})

# Merge
observeEvent(input$mergeTrack_x, {
  ids <- c("", theTracks()[ignore == FALSE & frame == input$videoPos_x]$track_fixed)

  showModal(
    modalDialog(
      title = "Merge tracks",
      easyClose = TRUE,

      tags$table(
        style = "width: 100%;",
        tags$tr(
          tags$td(selectInput("mergeID1", "Select first track", ids,
                              selected = NA, width = "100%"),
                  class = "halfWidth"),
          tags$td(selectInput("mergeID2", "Select second track", ids,
                              selected = NA, width = "100%"),
                  class = "halfWidth")
        )
      ),
      tags$p("Note: the left track ID will be kept.", class = "good"),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("okMerge", "Merge")
      )
    )
  )
})

observeEvent(input$okMerge, {
  removeModal(session)

  id1 <- as.numeric(input$mergeID1)
  id2 <- as.numeric(input$mergeID2)

  if (!is.na(id1) & !is.na(id2)) {
    idx <- theTracks()$track_fixed == id1 | theTracks()$track_fixed == id2
    orig <- theTracks()[idx]
    n <- names(orig)
    unit_real <- gsub("x", "", n[grepl("x_", n)])
    fixed <- orig[, {
      ix <- track_fixed == id1
      l <- list()

      if (.N == 2) {
        pts_px <- rbind(ellipse(x[1], y[1], width[1], height[1], angle[1]),
                        ellipse(x[2], y[2], width[2], height[2], angle[2]))
        ell_px <- amvee(pts_px)

        l[["track"]] <- c(id1, id2)
        l[["x"]] <- c(ell_px[1], x[!ix])
        l[["y"]] <- c(ell_px[2], y[!ix])
        l[["width"]] <- c(ell_px[3], width[!ix])
        l[["height"]] <- c(ell_px[4], height[!ix])
        l[["angle"]] <- c(ell_px[5], angle[!ix])
        l[["n"]] <- c(sum(n), n[!ix])

        if (length(unit_real) > 0) {
          pts_real <- rbind(ellipse(get(paste0("x", unit_real))[1],
                                    get(paste0("y", unit_real))[1],
                                    get(paste0("width", unit_real))[1],
                                    get(paste0("height", unit_real))[1],
                                    angle[1]),
                            ellipse(get(paste0("x", unit_real))[2],
                                    get(paste0("y", unit_real))[2],
                                    get(paste0("width", unit_real))[2],
                                    get(paste0("height", unit_real))[2],
                                    angle[2]))
          ell_real <- amvee(pts_real)

          l[[paste0("x", unit_real)]] <- c(ell_real[1], x[!ix])
          l[[paste0("y", unit_real)]] <- c(ell_real[2], y[!ix])
          l[[paste0("width", unit_real)]] <- c(ell_real[3],
                                               get(paste0("width", unit_real))[!ix])
          l[[paste0("height", unit_real)]] <- c(ell_real[4],
                                                get(paste0("height", unit_real))[!ix])
        }

        l[["track_fixed"]] <- c(id1, id2)
        l[["ignore"]] <- c(FALSE, TRUE)
      } else {
        l[["track"]] <- track
        l[["x"]] <- x
        l[["y"]] <- y
        l[["width"]] <- width
        l[["height"]] <- height
        l[["angle"]] <- angle
        l[["n"]] <- n

        if (length(unit_real) > 0) {
          l[[paste0("x", unit_real)]] <- get(paste0("x", unit_real))
          l[[paste0("y", unit_real)]] <- get(paste0("y", unit_real))
          l[[paste0("width", unit_real)]] <- get(paste0("width", unit_real))
          l[[paste0("height", unit_real)]] <- get(paste0("height", unit_real))
        }

        if (ix) {
          l[["track_fixed"]] <- track_fixed
        } else {
          l[["track_fixed"]] <- id1
        }

        l[["ignore"]] <- ignore
      }

      l
    },
    by = frame]

    theTracks()[idx, names(fixed) := fixed]

    changes[[length(changes) + 1]] <<- list(frame = input$videoPos_x,
                                            type = "merge",
                                            idx = which(idx),
                                            revert = orig)
    refreshStats(refreshStats() + 1)
    refreshDisplay(refreshDisplay() + 1)
  }
})

# Undo
observeEvent(input$revertChanges_x, {
  if (length(changes) > 0) {
    l <- length(changes)
    tmp <- theTracks()

    if (changes[[l]]$type == "reassign") {
      theTracks()[changes[[l]]$idx, track_fixed := changes[[l]]$revert]
    } else if (changes[[l]]$type == "remove") {
      theTracks()[changes[[l]]$idx, ignore := changes[[l]]$revert]
    } else if (changes[[l]]$type == "swap") {
      theTracks()[changes[[l]]$idx1, track_fixed := changes[[l]]$revert1]
      theTracks()[changes[[l]]$idx2, track_fixed := changes[[l]]$revert2]
    } else if (changes[[l]]$type == "merge") {
      theTracks()[changes[[l]]$idx, names(changes[[l]]$revert) := changes[[l]]$revert]
    }

    updateSliderInput(session, "videoPos_x", value = changes[[l]]$frame)
    changes[[l]] <<- NULL
    refreshStats(refreshStats() + 1)
    refreshDisplay(refreshDisplay() + 1)
  }
})

# Statistics
refreshStats <- reactiveVal(0)

output$trackStats <- renderTable({
  if (is.data.frame(theTracks()) & refreshStats() >= 0) {
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

# Save
observeEvent(input$saveChanges_x, {
  if (is.data.frame(theTracks())) {
    path <- parseFilePaths(roots = volumes, input$trackFile_x)
    fixed_path <- paste0(sub(".csv|_fixed.csv", "", path$datapath), "_fixed.csv")
    data.table::fwrite(theTracks(), fixed_path)
    showNotification(paste0("Changes saved at ", fixed_path),
                     id = "save", duration = 2)
  }
})
