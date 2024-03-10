#--------------------------------------------------------------
# Server Variables
#--------------------------------------------------------------
scale_coords <- NULL


#--------------------------------------------------------------
# Reactive Variables
#--------------------------------------------------------------
the_tracks_path <- reactiveVal()
scale_PX <- reactiveVal()
scale_REAL <- reactiveVal()
scale_modal <- reactiveVal(0)
origin <- reactiveVal(c(1, 1))
collect_scale <- reactiveVal(0)
stop_scale_collection <- reactiveVal(0)


#--------------------------------------------------------------
# Status Outputs
#--------------------------------------------------------------
output$scale_status <- renderUI({
  if (!is.null(origin)) {
    origin_st <- paste0(
      "Origin: [", round(origin()[1], 2),
      ",", round(origin()[2], 2), "]. "
    )
  } else {
    origin_st <- "No set origin (optional). "
  }

  scale_st <- "No set scale (optional)."

  if (!is.null(scale_PX()) & !is.null(scale_REAL())) {
    if (!is.na(scale_REAL())) {
      scale_st <- paste0(
        "1 ", input$unit_real, " = ",
        round(scale_PX() / scale_REAL(), 2), " pixels."
      )
    }
  }

  p(paste0(origin_st, scale_st), style = "text-align: center;")
})


#--------------------------------------------------------------
# UI Outputs
#--------------------------------------------------------------
observeEvent(scale_modal(), {
  if (!is.null(scale_PX()) & scale_modal() > 0) {
    units <- c("µm", "mm", "cm", "dm", "m", "km", "parsec")

    showModal(
      modalDialog(
        title = "Set scale",
        easyClose = TRUE,
        tags$table(
          style = "width: 100%;",
          tags$tr(
            tags$td(
              numericInput("scale_REAL",
                "Distance between the 2 reference points",
                NA, 0, Inf,
                width = "100%"
              )
            ),
            tags$td(style = "width: 10px;"),
            tags$td(
              selectInput("unit_real", "Unit", units,
                selected = "cm", width = "100px"
              ),
              style = "padding-top: 4px;"
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_scale", "Set Scale")
        )
      )
    )
  }
})

observeEvent(input$ok_scale, {
  removeModal(session)
})


#--------------------------------------------------------------
# Events
#--------------------------------------------------------------
observeEvent(input$main, {
  if (input$main == "6") {
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$origin_x, {
  if (isImage(the_image)) {
    showNotification("Select a point to set the origin.",
      id = "scale_notif", duration = NULL,
      type = "message"
    )

    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")
    collect_scale(1)
  }
})

observeEvent(input$scale_x, {
  if (isImage(the_image)) {
    showNotification("Select 2 reference points for calculating the scale.",
      id = "scale_notif", duration = NULL,
      type = "message"
    )

    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")
    collect_scale(2)
  }
})

observeEvent(input$plot_click, {
  if (collect_scale() > 0) {
    clck <- input$plot_click$coords_img
    clck$y <- -clck$y + nrow(the_mask) + 1
    scale_coords <<- rbind(scale_coords, unlist(clck))

    if (collect_scale() == 1 & nrow(scale_coords) >= 1) {
      stop_scale_collection(stop_scale_collection() + 1)
    }

    if (collect_scale() == 2 & nrow(scale_coords) >= 2) {
      stop_scale_collection(stop_scale_collection() + 1)
    }

    refresh_display(refresh_display() + 1)
  }
})

observeEvent(stop_scale_collection(), {
  if (collect_scale() > 0) {
    if (collect_scale() == 1) {
      origin(c(scale_coords[1], scale_coords[2]))
    } else {
      scale_PX(sqrt(diff(scale_coords[, 1])^2 + diff(scale_coords[, 2])^2))
      scale_modal(scale_modal() + 1)
    }

    removeNotification(id = "scale_notif")
    toggleInputs("ON")
    toggleTabs(1:6, "ON")
    collect_scale(0)
    scale_coords <<- NULL
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$scale_REAL, {
  scale_REAL(input$scale_REAL)
  refresh_display(refresh_display() + 1)
})

observeEvent(refresh_display(), {
  if (input$main == "6") {
    if (isImage(the_image)) {
      to_display <- cloneImage(the_image)
      sc <- max(dim(to_display) / 720)
      r <- 0.01 * min(nrow(to_display), ncol(to_display))

      avg_rgb <- mean(to_display) / 255
      avg_rgb1 <- avg_rgb == min(avg_rgb)
      avg_rgb2 <- avg_rgb == max(avg_rgb)
      color1 <- rgb(avg_rgb1[3], avg_rgb1[2], avg_rgb1[1])
      color2 <- rgb(avg_rgb2[3], avg_rgb2[2], avg_rgb2[1])

      if (!is.null(scale_PX()) & !is.null(scale_REAL())) {
        if (!is.na(scale_REAL())) {
          drawLine(to_display, origin()[1], origin()[2],
            origin()[1] + scale_PX() / scale_REAL(), origin()[2],
            color = color1, thickness = max(1, 1.5 * sc)
          )
          drawLine(to_display, origin()[1], origin()[2],
            origin()[1], origin()[2] + scale_PX() / scale_REAL(),
            color = color1, thickness = max(1, 1.5 * sc)
          )
          drawText(to_display, paste0(1, input$unit_real),
            origin()[1] + 6 * sc, origin()[2] + 6 * sc,
            font_scale = max(0.5, 0.5 * sc), thickness = max(1, sc),
            color = color1
          )
        }
      }

      if (!is.null(origin())) {
        drawCircle(to_display,
          x = origin()[1], y = origin()[2],
          radius = r * 1.5, thickness = -1, color = color1
        )
        drawCircle(to_display,
          x = origin()[1], y = origin()[2],
          radius = r, thickness = -1, color = color2
        )
      }

      if (collect_scale() == 2) {
        drawCircle(to_display,
          x = scale_coords[, 1], y = scale_coords[, 2],
          radius = r * 1.5, thickness = -1, color = color1
        )
        drawCircle(to_display,
          x = scale_coords[, 1], y = scale_coords[, 2],
          radius = r, thickness = -1, color = color2
        )
      }

      suppressMessages(
        write.Image(to_display, paste0(tmp_dir, "/display.jpg"), TRUE)
      )
    } else {
      suppressMessages(write.Image(
        zeros(1080, 1920, 3),
        paste0(tmp_dir, "/display.jpg"), TRUE
      ))
    }

    print_display(print_display() + 1)
  }
})

shinyFileSave(input, "compute_tracks_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$compute_tracks_x, {
  path <- parseSavePath(volumes, input$compute_tracks_x)
  the_tracks_path(path$datapath)
})


#--------------------------------------------------------------
# Tracking Loop
#--------------------------------------------------------------
max_dist <- NULL
memory <- NULL
memory_length <- NULL
mt <- NULL
n <- NULL
sc <- NULL
background <- NULL
mask <- NULL
pb <- NULL
old_check <- NULL
old_frame <- NULL
old_time <- NULL
max_width <- NULL
max_height <- NULL
min_density <- NULL
min_size <- NULL
centers <- NULL
frame <- NULL
proc_frame <- NULL
bw <- NULL
cc_dump <- NULL
avg_rgb <- NULL
color <- NULL
loop <- reactiveVal(0)
loop_D <- debounce(loop, 1)

observeEvent(the_tracks_path(), {
  if (isVideoStack(the_video) & isImage(the_background) &
    length(the_tracks_path()) > 0) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")

    showNotification("Tracking.", id = "tracking", duration = NULL)

    max_dist <<- input$maxDist_x
    memory <<- data.table(
      x = double(), y = double(), n = double(),
      frame = double(), id = integer(), track = integer(),
      width = double(), height = double(), angle = double()
    )
    memory_length <<- input$look_back_x
    mt <<- 0
    n <<- diff(input$range_pos_x) + 1
    sc <<- max(dim(the_background)) / 720

    background <<- cloneImage(the_background)
    changeColorSpace(background, "GRAY", "self")

    mask <<- cloneImage(the_mask)
    changeColorSpace(mask, "GRAY", "self")
    mask %i>% 0
    mask %i/% 255

    if (input$dark_button_x == "Darker") {
      not(background, target = "self")
    }

    pb <<- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    old_check <<- 0
    old_frame <<- 1
    old_time <<- Sys.time()

    max_width <<- input$blob_width_x
    max_height <<- input$blob_height_x
    min_density <<- input$blob_density_x
    min_size <<- input$blob_area_x

    centers <<- NULL

    frame <<- readFrame(the_video, input$range_pos_x[1])
    proc_frame <<- changeColorSpace(frame, "GRAY")
    bw <<- zeros(nrow(background), ncol(background), 1)
    cc_dump <<- zeros(nrow(background), ncol(background), 1, "16U")

    if (useGPU(proc_frame)) {
      proc_frame$toGPU()
      bw$toGPU()
      background$toGPU()
      mask$toGPU()
    }

    avg_rgb <<- mean(frame) / 255
    avg_rgb <<- avg_rgb == min(avg_rgb)
    color <<- rgb(avg_rgb[3], avg_rgb[2], avg_rgb[1])

    frame(the_video) <- input$range_pos_x[1] - 1

    loop(1)
  }
})

observeEvent(loop_D(), {
  if (loop() > 0) {
    if (loop() <= n) {
      readNext(the_video, target = frame)
      changeColorSpace(frame, "GRAY", proc_frame)

      if (input$show_tracks_x == "Yes") {
        display_frame <- cloneImage(frame)
      }

      if (input$dark_button_x == "Darker") {
        not(proc_frame, target = "self")
      }

      if (input$dark_button_x == "A bit of both") {
        absdiff(proc_frame, background, "self")
      } else {
        proc_frame %i-% background
      }

      proc_frame %i*% mask
      compare(proc_frame, input$threshold_x, ">=", bw)
      # boxFilter(bw, 1, 1, target = "self")
      # bw %i>% 63

      nz <- as.data.table(
        connectedComponents(bw, 8, target = cc_dump, stats = FALSE)$table
      )
      setcolorder(nz, c("label", "x", "y"))

      if (is.null(centers)) {
        centers <<- nz[, .(x = mean(x), y = mean(y)), by = .(label)][, 2:3]
      }

      d <- Rfast::dista(nz[, 2:3], centers)
      nz[, c("k", "kd") := list(Rfast::rowMins(d), Rfast::rowMins(d, value = TRUE))]
      nz[kd > (2 * max_height), "k"] <- NA
      gr <- unique(nz[, .(label, k)])
      setorder(gr, label)
      gr[, new_id := label]

      for (j in seq_len(nrow(gr))) {
        friends <- gr$new_id[gr$k == gr$k[j]]
        gr$new_id[gr$new_id %in% friends] <- gr$new_id[j]
      }

      uid <- unique(gr$new_id)
      nz <- as.matrix(nz)
      gr <- as.matrix(gr)

      shape <- c()

      for (j in seq_along(uid)) {
        ix <- gr[, 3] == uid[j]
        ugr <- unique(gr[ix, 2])
        pos <- nz[nz[, 1] %in% gr[ix, 1], 2:3]

        if (any(is.na(ugr))) {
          cl <- kbox(pos, 1,
            iter.max = 1000,
            split = TRUE, split.width = max_width,
            split.height = max_height,
            split.density = min_density,
            min.size = min_size
          )
        } else {
          cl <- kbox(pos, centers[ugr, , drop = FALSE],
            iter.max = 1000,
            split = TRUE, split.width = max_width,
            split.height = max_height,
            split.density = min_density,
            min.size = min_size
          )
        }

        shape <- rbind(shape, cl)
      }

      centers <<- shape[, 1:2, drop = FALSE]

      if (!is.null(shape)) {
        blobs <- data.table(
          x = shape[, 1],
          y = shape[, 2],
          n = shape[, 6],
          frame = frame(the_video),
          id = seq_len(nrow(shape)),
          track = NA,
          width = shape[, 3],
          height = shape[, 4],
          angle = shape[, 5]
        )

        if (input$do_not_track_x == "No") {
          memory <<- memory[frame >= (frame(the_video) - memory_length)]
          blobs <- simplerTracker(blobs, memory, maxDist = max_dist)
          new_track <- is.na(blobs$track)

          if (sum(new_track) > 0) {
            blobs$track[new_track] <- seq(mt + 1, mt + sum(new_track), 1)
            mt <<- mt + sum(new_track)
          }

          memory <<- rbind(memory, blobs)
        } else {
          blobs$track <- 1:nrow(blobs)
        }

        to_write <- blobs[, -"id"]
        setcolorder(
          to_write,
          c("frame", "track", "x", "y", "width", "height", "angle", "n")
        )

        if (!is.null(scale_PX()) & !is.null(scale_REAL())) {
          if (!is.na(scale_REAL())) {
            to_write[
              ,
              paste0(c("x", "y", "width", "height"), "_", input$unit_real) :=
                .(
                  (x - origin()[1]) * scale_REAL() / scale_PX(),
                  (y - origin()[2]) * scale_REAL() / scale_PX(),
                  width * scale_REAL() / scale_PX(),
                  height * scale_REAL() / scale_PX()
                )
            ]
          }
        }

        if (loop() == 1) {
          if (file.exists(the_tracks_path())) {
            unlink(the_tracks_path())
          }
          fwrite(to_write, the_tracks_path(), append = FALSE)
        } else {
          fwrite(to_write, the_tracks_path(), append = TRUE)
        }

        if (input$show_tracks_x == "Yes") {
          drawRotatedRectangle(display_frame, blobs$x, blobs$y, blobs$width,
            blobs$height, blobs$angle,
            color = color,
            thickness = max(1, 1.5 * sc)
          )
          drawText(display_frame, blobs$track,
            blobs$x - (floor(log10(blobs$track)) + 1) * 5 * sc,
            blobs$y - 5 * sc,
            font_scale = 0.5 * sc,
            thickness = max(1, 1.5 * sc), color = color
          )
        }
      }

      if (input$show_tracks_x == "Yes") {
        suppressMessages(
          write.Image(display_frame, paste0(tmp_dir, "/display.jpg"), TRUE)
        )
        print_display(print_display() + 1)
      }

      new_check <- floor(100 * loop() / n)
      if (new_check > old_check) {
        new_time <- Sys.time()
        fps <- (loop() - old_frame + 1) /
          as.numeric(difftime(new_time, old_time, units = "secs"))
        old_check <<- new_check
        old_frame <<- loop()
        old_time <<- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(new_check, "% - ", round(fps, digits = 2), "fps")
        )
      }

      loop(loop() + 1)
    } else {
      loop(0)
      pb$close()
      removeNotification(id = "tracking")
      toggleInputs("ON")
      toggleTabs(1:6, "ON")
    }
  }
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(
  session$getBookmarkExclude(), "compute_tracks_x", "scale_x",
  "origin_x", "ok_scale"
))

onBookmark(function(state) {
  state$values$scale_PX <- scale_PX()
  state$values$scale_REAL <- scale_REAL()
  state$values$origin <- origin()
})

onRestore(function(state) {
  scale_PX(state$values$scale_PX)
  scale_REAL(state$values$scale_REAL)
  origin(state$values$origin)
})
