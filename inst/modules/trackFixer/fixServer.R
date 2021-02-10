####################
# GUI
####################

# Enable panel
observe({
  if (Rvision::isVideo(theVideo()) & is.data.frame(theTracks())) {
    enable(selector = "a[data-value=2]")
    updateVerticalTabsetPanel(session, "main", selected = "2")
  }
})

# Status
output$videoStatus2 <- renderUI({
  if (!Rvision::isVideo(theVideo())) {
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
  if (Rvision::isVideo(theVideo())) {
    sliderInput("videoSize_x", "Display size", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

# Video slider
output$videoSlider <- renderUI({
  if (Rvision::isVideo(theVideo())) {
    if (is.data.frame(theTracks())) {
      sliderInput("videoPos_x", NULL, width = "100%",
                  value = min(theTracks()[, frame]), min = 1,
                  max = Rvision::nframes(theVideo()), step = 1)
    } else {
      sliderInput("videoPos_x", NULL, width = "100%", value = 1, min = 1,
                  max = Rvision::nframes(theVideo()), step = 1)
    }
  }
})

# Controls
play <- reactiveVal(FALSE)

observeEvent(input$playPause_x, {
  if (!play() & Rvision::isVideo(theVideo())) {
    play(TRUE)
  } else {
    play(FALSE)
  }
})

observeEvent(input$videoSize_x, {
  theImage(Rvision::readFrame(theVideo(), input$videoPos_x))
})

observeEvent(input$videoPos_x, {
  if (input$videoPos_x - theVideo()$frame() == 1) {
    theImage(Rvision::readNext(theVideo()))
  } else {
    theImage(Rvision::readFrame(theVideo(), input$videoPos_x))
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
  if (Rvision::isImage(theImage()) & is.data.frame(theTracks())) {
    isolate( refreshDisplay(refreshDisplay() + 1) )
  }
})

observeEvent(refreshDisplay(), {
  if (refreshDisplay() > 0) {
    tmp_rect <- theTracks()[ignore == FALSE & frame == input$videoPos_x, ]

    if (nrow(tmp_rect) > 0) {
      sc <- max(dim(theImage()) / 720)

      overlay1 <- Rvision::cloneImage(theImage())
      Rvision::drawRotatedRectangle(overlay1, tmp_rect$x, tmp_rect$y,
                                    tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                    color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                    thickness = 1.5 * sc)

      overlay2 <- Rvision::cloneImage(theImage())
      Rvision::drawRotatedRectangle(overlay2, tmp_rect$x, tmp_rect$y,
                                    tmp_rect$width, tmp_rect$height, tmp_rect$angle,
                                    color = cbPalette[(tmp_rect$track_fixed %% 12) + 1],
                                    thickness = -1)

      tmp_tracks <- theTracks()[ignore == FALSE &
                                  frame >= (input$videoPos_x - 1 * theVideo()$fps()) &
                                  frame <= input$videoPos_x, ]

      tmp_tracks[, Rvision::drawPolyline(overlay2, cbind(x, y), FALSE,
                                         color = cbPalette[(track_fixed[1] %% 12) + 1],
                                         thickness = 3 * sc),
                 by = track_fixed]

      to_display <- Rvision::addWeighted(overlay1, overlay2, c(0.5, 0.5))
      Rvision::drawText(to_display, tmp_rect$track_fixed,
                        tmp_rect$x - (floor(log10(tmp_rect$track_fixed)) + 1) * 5 * sc,
                        tmp_rect$y - 5 * sc, font_scale = 0.5 * sc, thickness = 1.5 * sc,
                        color = "white")

      Rvision::display(to_display, "trackR", 5,
                       nrow(to_display) * input$videoSize_x,
                       ncol(to_display) * input$videoSize_x)
    } else {
      Rvision::display(theImage(), "trackR", 5,
                       nrow(theImage()) * input$videoSize_x,
                       ncol(theImage()) * input$videoSize_x)
    }
  } else {
    Rvision::display(Rvision::zeros(480, 640), "trackR", 5, 480, 640)
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
    idx <- theTracks()[, track_fixed] == rm_id
    theTracks()[idx, ignore := TRUE]
    changes[[length(changes) + 1]] <<- list(frame = input$videoPos_x,
                                            type = "remove",
                                            idx = which(idx),
                                            revert = FALSE)
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

  id1 <- as.integer(input$mergeID1)
  id2 <- as.integer(input$mergeID2)

  if (!is.na(id1) & !is.na(id2)) {
    idx <- theTracks()$track == id1 | theTracks()$track == id2
    orig <- theTracks()[idx]
    fixed <- orig[, {
      ix <- track == id1

      if (.N == 2) {
        pts <- rbind(ellipse(x[1], y[1], width[1], height[1], angle[1]),
                     ellipse(x[2], y[2], width[2], height[2], angle[2]))
        ell <- amvee(pts)
        list(x = c(ell[1], x[!ix]),
             y = c(ell[2], y[!ix]),
             n = c(sum(n), n[!ix]),
             track = c(id1, id2),
             width = c(ell[3], width[!ix]),
             height = c(ell[4], height[!ix]),
             angle = c(ell[5], angle[!ix]),
             track_fixed = c(id1, id2),
             ignore = c(FALSE, TRUE))
      } else {
        if (ix) {
          list(x, y, n, track, width, height, angle, track_fixed, ignore)
        } else {
          list(x, y, n, track, width, height, angle, track_fixed = id1, ignore)
        }
      }
    },
    by = frame]

    theTracks()[idx, names(fixed) := fixed]

    changes[[length(changes) + 1]] <<- list(frame = input$videoPos_x,
                                            type = "merge",
                                            idx = which(idx),
                                            revert = orig)
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
    refreshDisplay(refreshDisplay() + 1)
  }
})

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
