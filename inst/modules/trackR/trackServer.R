# Variables and reactives
theTracksPath <- reactiveVal()
scalePX <- reactiveVal()
scaleREAL <- reactiveVal()
scaleModal <- reactiveVal(0)
origin <- reactiveVal(c(1, 1))


# Output
output$trackingStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (refreshDisplay() > -1 & !isImage(theBackground)) {
    p("Background missing (and required).", class = "bad")
  }
})

output$scaleStatus <- renderUI({
  if (!is.null(origin)) {
    origin_st <- paste0("Origin: [", origin()[1], ",", origin()[2], "]. ")
  } else {
    origin_st <- "No set origin (optional). "
  }

  scale_st <- "No set scale (optional)."

  if (!is.null(scalePX()) & !is.null(scaleREAL())) {
    if (!is.na(scaleREAL())) {
      scale_st <- paste0("1 ", input$unitReal, " = ", round(scalePX() / scaleREAL(), 2), " pixels.")
    }
  }

  p(paste0(origin_st, scale_st), style = "text-align: center;")
})


# Events
observeEvent(input$main, {
  if (input$main == "6") {
    refreshDisplay(refreshDisplay() + 1)
  }
})

observeEvent(input$origin_x, {
  if (isImage(theImage)) {
    toggleAll("OFF")

    displayOrigin <- cloneImage(theImage)

    showNotification("Select a point to set the origin.",
                     id = "origin_notif", duration = NULL,
                     type = "message")

    POI <- data.frame()

    r <- 0.01 * min(nrow(displayOrigin), ncol(displayOrigin))

    POI <- rbind(POI, click(displayOrigin, input$videoSize_x, "trackR"))
    drawCircle(displayOrigin, x = POI$x, y = POI$y,
               radius = r * 1.5, thickness = -1, color = "white")
    drawCircle(displayOrigin, x = POI$x, y = POI$y,
               radius = r, thickness = -1, color = "red")
    display(displayOrigin, window_name = "trackR", delay = 25,
            height = nrow(displayOrigin) * input$videoSize_x,
            width = ncol(displayOrigin) * input$videoSize_x)

    origin(c(POI$x, POI$y))

    removeNotification(id = "origin_notif")

    toggleAll("ON")
  }
})

observeEvent(input$scale_x, {
  if (isImage(theImage)) {
    toggleAll("OFF")

    displayScale <- cloneImage(theImage)

    showNotification("Select 2 reference points for calculating the scale.",
                     id = "scale_notif", duration = NULL,
                     type = "message")

    POI <- data.frame()

    r <- 0.01 * min(nrow(displayScale), ncol(displayScale))

    for (i in 1:2) {
      POI <- rbind(POI, click(displayScale, input$videoSize_x, "trackR"))
      if (i == 2)
        drawLine(displayScale, pt1_x = POI$x[1], pt1_y = POI$y[1],
                 pt2_x = POI$x[2], pt2_y = POI$y[2], thickness = r / 2,
                 color = "white")
      drawCircle(displayScale, x = POI$x, y = POI$y,
                 radius = r * 1.5, thickness = -1, color = "white")
      drawCircle(displayScale, x = POI$x, y = POI$y,
                 radius = r, thickness = -1, color = "red")
      display(displayScale, window_name = "trackR", delay = 25,
              height = nrow(displayScale) * input$videoSize_x,
              width = ncol(displayScale) * input$videoSize_x)
    }

    scalePX(sqrt(diff(POI$x) ^ 2 + diff(POI$y) ^ 2))

    removeNotification(id = "scale_notif")
    toggleAll("ON")
    scaleModal(scaleModal() + 1)
  }
})

observeEvent(scaleModal(), {
  if (!is.null(scalePX()) & scaleModal() > 0) {
    units <- c("µm", "mm", "cm", "dm", "m", "km", "parsec")

    showModal(
      modalDialog(
        title = "Set scale",
        easyClose = TRUE,

        tags$table(
          style = "width: 100%;",
          tags$tr(
            tags$td(numericInput("scaleREAL", "Distance between the 2 reference points",
                                 NA, 0, Inf, width = "100%")),
            tags$td(style = "width: 10px;"),
            tags$td(selectInput("unitReal", "Unit", units, selected = "cm", width = "100px"),
                    style = "padding-top: 4px;")
          )
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton("okScale", "Set Scale")
        )
      )
    )
  }
})

observeEvent(input$okScale, {
  removeModal(session)
})

observeEvent(input$scaleREAL, {
  scaleREAL(input$scaleREAL)
})

observeEvent(refreshDisplay(), {
  if (input$main == "6") {
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

shinyFileSave(input, "computeTracks_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$computeTracks_x, {
  path <- parseSavePath(volumes, input$computeTracks_x)
  theTracksPath(path$datapath)
})

observeEvent(theTracksPath(), {
  if (isVideo(theVideo) & isImage(theBackground) & length(theTracksPath()) > 0) {
    toggleAll("OFF")

    showNotification("Tracking.", id = "tracking", duration = NULL)

    max_dist <- input$maxDist_x
    memory <- data.table(x = double(), y = double(), n = double(),
                         frame = double(), id = integer(), track = integer(),
                         width = double(), height = double(), angle = double())
    memory_length <- input$lookBack_x
    mt <- 0
    n <- diff(input$rangePos_x) + 1
    sc <- max(dim(theBackground)) * input$videoQuality_x / 720

    background <- cloneImage(theBackground)

    if (!isImage(theMask)) {
      theMask <<- ones(nrow(theBackground), ncol(theBackground), 3)
      theMask %i*% 255
    }
    mask <- cloneImage(theMask)

    if (input$videoQuality_x < 1) {
      background <- resize(background, fx = input$videoQuality_x,
                           fy = input$videoQuality_x, interpolation = "area")
      mask <- resize(mask, fx = input$videoQuality_x, fy = input$videoQuality_x,
                     interpolation = "area")
    }

    if (input$darkButton_x == "Darker")
      not(background, target = "self")

    mask %i/% 255

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    # speedup <- input$speedup_x
    max_width <- input$blobWidth_x
    max_height <- input$blobHeight_x
    min_density <- input$blobDensity_x # / (input$speedup_x ^ 2)
    min_size <- input$blobArea_x # / (input$speedup_x ^ 2)

    centers <- NULL

    frame <- zeros(nrow(background), ncol(background), 3)
    proc_frame <- zeros(nrow(background), ncol(background), 3)
    bw <- zeros(nrow(background), ncol(background), 1)
    cc_dump <- zeros(nrow(background), ncol(background), 1, "32S")

    avg_rgb <- mean(frame) / 255
    avg_rgb <- avg_rgb == min(avg_rgb)
    color <- rgb(avg_rgb[3], avg_rgb[2], avg_rgb[1])

    setProp(theVideo, "POS_FRAMES", input$rangePos_x[1] - 1)

    for (i in 1:n) {
      if (input$videoQuality_x < 1) {
        readNext(theVideo, target = frame)
        resize(frame, fx = input$videoQuality_x, fy = input$videoQuality_x,
               interpolation = "area", target = proc_frame)
      } else {
        readNext(theVideo, target = proc_frame)
      }

      if (input$showTracks_x == "Yes")
        display_frame <- cloneImage(proc_frame)

      if (input$darkButton_x == "Darker")
        not(proc_frame, target = "self")

      proc_frame %i-% background
      proc_frame %i*% mask
      inRange(proc_frame, target = bw, c(input$blueThreshold_x,
                                         input$greenThreshold_x,
                                         input$redThreshold_x, 0))
      boxFilter(bw, target = "self")
      bw %i>% 63

      nz <- as.data.table(connectedComponents(bw, 8, target = cc_dump)$table)
      setcolorder(nz, c("id", "x", "y"))
      # nz <- nz[, if(.N >= 5) .SD, by = .(id)]
      # nz <- nz[(x %% speedup) == 0 & (y %% speedup) == 0]

      if (is.null(centers)) {
        centers <- nz[, .(x = mean(x), y = mean(y)), by = .(id)][, 2:3]
      }

      d <- Rfast::dista(nz[, 2:3], centers)
      nz[, c("k", "kd") := list(Rfast::rowMins(d), Rfast::rowMins(d, value = TRUE))]
      nz[kd > (2 * max_height), "k"] <- NA
      gr <- unique(nz[, .(id, k)])
      setorder(gr, id)
      gr[, new_id := id]

      for (j in 1:nrow(gr)) {
        friends <- gr$new_id[gr$k == gr$k[j]]
        gr$new_id[gr$new_id %in% friends] <- gr$new_id[j]
      }

      uid <- unique(gr$new_id)
      nz <- as.matrix(nz)
      gr <- as.matrix(gr)

      shape <- c()

      for (j in 1:length(uid)) {
        ix <- gr[, 3] == uid[j]
        ugr <- unique(gr[ix, 2])
        pos <- nz[nz[, 1] %in% gr[ix, 1], 2:3]

        if (any(is.na(ugr))) {
          cl <- kbox(pos, 1, iter.max = 1000,
                     split = TRUE, split.width = max_width,
                     split.height = max_height,
                     split.density = min_density,
                     min.size = min_size)
        } else {
          cl <- kbox(pos, centers[ugr, , drop = FALSE], iter.max = 1000,
                     split = TRUE, split.width = max_width,
                     split.height = max_height,
                     split.density = min_density,
                     min.size = min_size)
        }

        shape <- rbind(shape, cl)
      }

      centers <- shape[, 1:2, drop = FALSE]

      if (!is.null(shape)) {
        blobs <- data.table(x = shape[, 1],
                            y = shape[, 2],
                            n = shape[, 6],
                            frame = theVideo$frame(),
                            id = 1:nrow(shape),
                            track = NA,
                            width = shape[, 3],
                            height = shape[, 4],
                            angle = shape[, 5])

        if (input$doNotTrack_x == "No") {
          memory <- memory[frame >= (theVideo$frame() - memory_length)]
          blobs <- simplerTracker(blobs, memory, maxDist = input$maxDist_x)
          newTrack <- is.na(blobs$track)

          if (sum(newTrack) > 0) {
            blobs$track[newTrack] <- seq(mt + 1, mt + sum(newTrack), 1)
            mt <- mt + sum(newTrack)
          }

          memory <- rbind(memory, blobs)
        } else {
          blobs$track <- 1:nrow(blobs)
        }

        to_write <- blobs[, -"id"]
        setcolorder(to_write, c("frame", "track", "x", "y", "width", "height", "angle", "n"))

        if (input$videoQuality_x < 1) {
          to_write[, c("x", "y", "n", "width", "height") := .(x / input$videoQuality_x,
                                                              y / input$videoQuality_x,
                                                              n / (input$videoQuality_x ^ 2),
                                                              width / input$videoQuality_x,
                                                              height / input$videoQuality_x)]
        }

        if (!is.null(scalePX()) & !is.null(scaleREAL())) {
          if (!is.na(scaleREAL())) {
            to_write[, paste0(c("x", "y", "width", "height"), "_", input$unitReal) :=
                       .((x - origin()[1]) * scaleREAL() / scalePX(),
                         (y - origin()[2]) * scaleREAL() / scalePX(),
                         width * scaleREAL() / scalePX(),
                         height * scaleREAL() / scalePX())]
          }
        }

        if (i == 1) {
          if (file.exists(theTracksPath())) {
            unlink(theTracksPath())
          }
          fwrite(to_write, theTracksPath(), append = FALSE)
        } else {
          fwrite(to_write, theTracksPath(), append = TRUE)
        }

        if (input$showTracks_x == "Yes") {
          drawRotatedRectangle(display_frame, blobs$x, blobs$y, blobs$width,
                               blobs$height, blobs$angle, color = color,
                               thickness = max(1, 1.5 * sc))
          drawText(display_frame, blobs$track,
                   blobs$x - (floor(log10(blobs$track)) + 1) * 5 ,
                   blobs$y - 5 * sc, font_scale = 0.5 * sc,
                   thickness = max(1, 1.5 * sc), color = color)
        }
      }

      if (input$showTracks_x == "Yes") {
        display(display_frame, "trackR", 1,
                nrow(display_frame) * input$videoSize_x,
                ncol(display_frame) * input$videoSize_x)
      }

      new_check <- floor(100 * i / n)
      if (new_check > old_check) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(value = new_check / 100, detail = paste0(new_check, "% - ", round(fps, digits = 2), "fps"))
      }
    }

    pb$close()

    removeNotification(id = "tracking")
    toggleAll("ON")
  }
})


# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "computeTracks_x", "scale_x",
                     "origin_x", "okScale"))

onBookmark(function(state) {
  state$values$scalePX <- scalePX()
  state$values$scaleREAL <- scaleREAL()
  state$values$origin <- origin()
})

onRestore(function(state) {
  scalePX(state$values$scalePX)
  scaleREAL(state$values$scaleREAL)
  origin(state$values$origin)
})
