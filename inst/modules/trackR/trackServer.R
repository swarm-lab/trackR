theTracksPath <- reactiveVal()

# Status
output$trackingStatus <- renderUI({
  if (is.null(input$videoSize_x)) {
    p("Video missing (and required).", class = "bad")
  } else if (!Rvision::isImage(theBackground())) {
    p("Background missing (and required).", class = "bad")
  }
})

# Tracking
shinyFileSave(input, "computeTracks_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$computeTracks_x, {
  path <- parseSavePath(volumes, input$computeTracks_x)
  theTracksPath(path$datapath)
})

observeEvent(theTracksPath(), {
  if (Rvision::isVideo(theVideo()) & Rvision::isImage(theBackground()) & length(theTracksPath()) > 0) {
    toggleAll("OFF")

    showNotification("Tracking.", id = "tracking", duration = NULL)

    max_dist <- input$maxDist_x
    memory <- data.table::data.table(x = double(), y = double(), n = double(),
                                     frame = double(), id = integer(), track = integer(),
                                     width = double(), height = double(), angle = double())
    memory_length <- input$lookBack_x
    mt <- 0

    m_bg <- Rvision::sum(theBackground())

    n <- diff(input$rangePos_x) + 1

    sc <- max(dim(theBackground())) / 720

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    speedup <- input$speedup_x
    max_width <- input$blobWidth_x
    max_height <- input$blobHeight_x
    min_density <- input$blobDensity_x / input$speedup_x ^ 2
    min_size <- input$blobArea_x / input$speedup_x ^ 2

    for (i in 1:n) {
      if (i == 1) {
        frame <- Rvision::readFrame(theVideo(), input$rangePos_x[1])
      } else {
        frame <- Rvision::readNext(theVideo())
      }

      m_fr <- Rvision::sum(frame)
      r <- mean(m_bg / m_fr)

      if (input$darkButton_x == "Darker") {
        d <- (theBackground() - (frame * r)) * (theMask() / 255)
      } else {
        d <- ((frame * r) - theBackground()) * (theMask() / 255)
      }

      bw <- Rvision::inRange(d, c(input$blueThreshold_x, input$greenThreshold_x,
                                  input$redThreshold_x, 0))
      Rvision::boxFilter(bw, in_place = TRUE)

      nz <- data.table::as.data.table(Rvision::connectedComponents(bw > 63, 8)$table)
      nz <- nz[(x %% speedup) == 0 & (y %% speedup) == 0]

      if (i == 1) {
        centers <- nz[, .(x = mean(x), y = mean(y)), by = .(id)][, 2:3]
      }

      d <- Rfast::dista(nz[, 2:3], centers)
      nz[, k := Rfast::rowMins(d)]
      gr <- nz[, .N, by = .(id, k)]
      data.table::setorder(gr, id)
      gr[, new_id := id]
      gr <- gr[N > min_size, ]

      for (j in 1:nrow(gr)) {
        friends <- gr$new_id[gr$k == gr$k[j]]
        gr$new_id[gr$new_id %in% friends] <- gr$new_id[j]
      }

      uid <- unique(gr$new_id)
      nz <- as.matrix(nz)
      gr <- as.matrix(gr)

      shape <- c()

      for (j in 1:length(uid)) {
        ix <- gr[, 4] == uid[j]
        ugr <- unique(gr[ix, 2])
        pos <- nz[nz[, 1] %in% gr[ix, 1], 2:3]
        cl <- kbox(pos, centers[ugr, , drop = FALSE], iter.max = 1000,
                   split = TRUE, split.width = max_width,
                   split.height = max_height,
                   split.density = min_density,
                   min.size = min_size)

        shape <- rbind(shape, cl)
      }

      centers <- shape[, 1:2]

      blobs <- data.table::data.table(x = shape[, 1],
                                      y = shape[, 2],
                                      n = shape[, 6],
                                      frame = frame(theVideo()),
                                      id = 1:nrow(shape),
                                      track = NA,
                                      width = shape[, 3],
                                      height = shape[, 4],
                                      angle = shape[, 5])

      if (nrow(blobs) > 0) {
        memory <- memory[frame >= (frame(theVideo()) - memory_length)]
        blobs <- simplerTracker(blobs, memory, maxDist = input$maxDist_x)
        newTrack <- is.na(blobs$track)

        if (sum(newTrack) > 0) {
          blobs$track[newTrack] <- seq(mt + 1, mt + sum(newTrack), 1)
          mt <- mt + sum(newTrack)
        }

        memory <- rbind(memory, blobs)

        if (i == 1) {
          if (file.exists(theTracksPath())) {
            unlink(theTracksPath())
          }
          readr::write_csv(blobs[, -"id"], theTracksPath(), append = FALSE)
        } else {
          readr::write_csv(blobs[, -"id"], theTracksPath(), append = TRUE)
        }

        if (input$showTracks_x == "Yes") {
          Rvision::drawRotatedRectangle(frame, blobs$x, blobs$y, blobs$width,
                                        blobs$height, blobs$angle, color = "green",
                                        thickness = 2)
          Rvision::drawText(frame, blobs$track,
                            blobs$x - (floor(log10(blobs$track)) + 1) * 4 ,
                            blobs$y - 4 * sc, font_scale = 0.4 * sc,
                            thickness = 1.5 * sc, color = "green")

          Rvision::display(
            Rvision::resize(frame, fx = input$videoQuality_x,
                            fy = input$videoQuality_x, interpolation = "area"),
            "trackR", 1,
            nrow(frame) * input$videoSize_x,
            ncol(frame) * input$videoSize_x)
        }
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
setBookmarkExclude(c(session$getBookmarkExclude(), "computeTracks_x"))
