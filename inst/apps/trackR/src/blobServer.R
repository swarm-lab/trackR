#--------------------------------------------------------------
# Status Outputs
#--------------------------------------------------------------
observeEvent(print_display(), {
  if (is.null(input$video_pos3_x)) {
    toggleTabs(6, "OFF")
    disable(selector = "[data-value='6']")
  } else {
    toggleTabs(6, "ON")
  }
})


#--------------------------------------------------------------
# UI Outputs
#--------------------------------------------------------------
output$video_slider3 <- renderUI({
  if (!is.null(input$range_pos_x)) {
    sliderInput("video_pos3_x", "Frame",
      width = "100%", step = 1,
      value = frame_mem,
      min = input$range_pos_x[1],
      max = input$range_pos_x[2]
    )
  }
})


#--------------------------------------------------------------
# Events
#--------------------------------------------------------------
observeEvent(input$main, {
  if (input$main == "5") {
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(refresh_display(), {
  if (isVideoStack(the_video) & input$blob_width_x == 0 &
    input$blob_height_x == 0 & input$blob_area_x == 0) {
    updateNumericInput(session, "blob_width_x",
      value = nrow(the_video),
      max = nrow(the_video)
    )
    updateNumericInput(session, "blob_height_x",
      value = ncol(the_video),
      max = nrow(the_video)
    )
    updateNumericInput(session, "blob_area_x",
      value = 1,
      max = nrow(the_video) * ncol(the_video)
    )
  }
})

observeEvent(input$blob_height_x, {
  updateNumericInput(session, "blob_width_x", max = input$blob_height_x)
})

observeEvent(input$optimize_blobs_x, {
  if (isVideoStack(the_video) & isImage(the_background)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")

    showNotification("Optimizing blob parameters.",
      id = "optim",
      duration = NULL
    )

    frame_pos <- round(seq.int(input$range_pos_x[1], input$range_pos_x[2],
      length.out = 100
    ))
    tot_summ <- NULL

    pb <- Progress$new()
    pb$set(message = "Computing: ", value = 0, detail = "0%")
    n <- length(frame_pos)
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    background <- cloneImage(the_background)
    changeColorSpace(background, "GRAY", "self")

    if (!isImage(the_mask)) {
      the_mask <<- ones(nrow(the_background), ncol(the_background), 3)
    }
    mask <- cloneImage(the_mask)
    changeColorSpace(mask, "GRAY", "self")

    if (input$dark_button_x == "Darker") {
      not(background, target = "self")
    }

    mask %i>% 0
    mask %i/% 255

    frame <- zeros(nrow(background), ncol(background), 3)
    proc_frame <- zeros(nrow(background), ncol(background), 1)
    bw <- zeros(nrow(background), ncol(background), 1)
    cc_dump <- zeros(nrow(background), ncol(background), 1, "32S")

    for (i in 1:n) {
      readFrame(the_video, frame_pos[i], frame)
      changeColorSpace(frame, "GRAY", proc_frame)

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
      boxFilter(bw, 1, 1, target = "self")
      bw %i>% 63

      nz <- as.data.table(connectedComponents(bw, 8, target = cc_dump)$table)
      setcolorder(nz, c("label", "x", "y"))
      nz_summ <- nz[, as.data.table(kbox(cbind(x, y))), by = .(label)]

      if (nrow(nz_summ) > 0) {
        nz_summ[, area := (width / 2) * (height / 2) * pi]
        nz_summ[, density := n / area]
        tot_summ <- rbindlist(list(tot_summ, nz_summ))
      }

      new_check <- floor(100 * i / n)
      if (new_check > (old_check + 5)) {
        new_time <- Sys.time()
        fps <- (i - old_frame + 1) / as.numeric(difftime(new_time, old_time,
          units = "secs"
        ))
        old_check <- new_check
        old_frame <- i
        old_time <- new_time
        pb$set(
          value = new_check / 100,
          detail = paste0(
            new_check, "% - ",
            round(fps, digits = 2), "fps"
          )
        )
      }
    }

    tot_summ[, outlier := mvoutlier::pcout(
      cbind(scale(width), scale(height), scale(density))
    )$wfinal01 == 0]

    pb$close()

    removeNotification(id = "optim")
    toggleInputs("ON")
    toggleTabs(1:5, "ON")

    updateNumericInput(session, "blob_width_x",
      value = round(1.05 * max(tot_summ[outlier == FALSE, width]))
    )
    updateNumericInput(session, "blob_height_x",
      value = round(1.05 * max(tot_summ[outlier == FALSE, height]))
    )
    updateNumericInput(session, "blob_area_x",
      value = round(0.95 * min(tot_summ[outlier == FALSE, n]))
    )
    updateNumericInput(session, "blob_density_x",
      value = round(0.95 * min(tot_summ[outlier == FALSE, density]), 3)
    )
  }
})

observeEvent(input$blob_width_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(input$blob_height_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(input$blob_area_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(input$blob_density_x, {
  refresh_display(refresh_display() + 1)
})

observeEvent(refresh_display(), {
  if (input$main == "5") {
    if (!isImage(the_image) & !isImage(the_background)) {
      suppressMessages(
        write.Image(
          zeros(1080, 1920, 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else if (!isImage(the_image)) {
      suppressMessages(
        write.Image(
          zeros(nrow(the_background), ncol(the_background), 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else if (!isImage(the_background)) {
      suppressMessages(
        write.Image(
          zeros(nrow(the_image), ncol(the_image), 3),
          paste0(tmp_dir, "/display.jpg"), TRUE
        )
      )
    } else {
      background <- cloneImage(the_background)
      changeColorSpace(background, "GRAY", "self")

      if (!isImage(the_mask)) {
        the_mask <<- ones(nrow(the_background), ncol(the_background), 3)
      }
      mask <- cloneImage(the_mask)
      changeColorSpace(mask, "GRAY", "self")

      if (input$dark_button_x == "Darker") {
        not(background, target = "self")
      }

      frame <- cloneImage(the_image)
      proc_frame <- changeColorSpace(frame, "GRAY")
      cc_dump <- zeros(nrow(background), ncol(background), 1, "32S")

      if (input$dark_button_x == "Darker") {
        not(proc_frame, target = "self")
      }

      if (input$dark_button_x == "A bit of both") {
        absdiff(proc_frame, background, "self")
      } else {
        proc_frame %i-% background
      }

      mask %i>% 0
      mask %i/% 255
      proc_frame %i*% mask
      to_display <- cloneImage(frame * (255 / max(max(frame))))
      bw <- compare(proc_frame, input$threshold_x, ">=")
      boxFilter(bw, 1, 1, target = "self")
      bw %i>% 63

      nz <- as.data.table(connectedComponents(bw, 8, target = cc_dump)$table)
      setcolorder(nz, c("label", "x", "y"))
      centers <- nz[, .(x = mean(x), y = mean(y)), by = .(label)]

      d <- Rfast::dista(nz[, 2:3], centers[, 2:3])
      nz[, k := Rfast::rowMins(d)]
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

      for (j in 1:length(uid)) {
        ix <- gr[, 3] == uid[j]
        ugr <- unique(gr[ix, 2])
        pos <- nz[nz[, 1] %in% gr[ix, 1], 2:3]
        cl <- kbox(pos, centers[ugr, 2:3, drop = FALSE],
          iter.max = 1000,
          split = TRUE,
          split.width = if (is.na(input$blob_width_x)) Inf else input$blob_width_x,
          split.height = if (is.na(input$blob_height_x)) Inf else input$blob_height_x,
          split.density = if (is.na(input$blob_density_x)) 0 else input$blob_density_x,
          min.size = if (is.na(input$blob_area_x)) 1 else input$blob_area_x
        )
        shape <- rbind(shape, cl)
      }

      sc <- max(dim(to_display) / 720)
      color <- "green"

      if (length(shape) > 0) {
        drawRotatedRectangle(to_display, shape[, 1], shape[, 2], shape[, 3],
          shape[, 4], shape[, 5],
          color = color,
          thickness = max(1, 1.5 * sc)
        )
      }

      x <- round(0.025 * ncol(to_display))
      y <- round(0.025 * nrow(to_display))
      avg_col <- mean(to_display[y:(y + 50), x:(x + 50), ])

      drawLine(to_display, 0.025 * ncol(to_display), 0.025 * nrow(to_display),
        0.025 * ncol(to_display) + 50, 0.025 * nrow(to_display),
        color = color, thickness = max(1, 1.5 * sc)
      )

      drawLine(to_display, 0.025 * ncol(to_display), 0.025 * nrow(to_display),
        0.025 * ncol(to_display), 0.025 * nrow(to_display) + 50,
        color = color, thickness = max(1, 1.5 * sc)
      )

      drawText(to_display, "50px", x + 6 * sc, y + 6 * sc,
        font_scale = max(0.5, 0.5 * sc),
        thickness = max(1, sc), color = color
      )

      suppressMessages(
        write.Image(to_display, paste0(tmp_dir, "/display.jpg"), TRUE)
      )
    }

    print_display(print_display() + 1)
  }
})

observeEvent(input$video_pos3_x, {
  if (input$main == "5") {
    updateSliderInput(session, "video_pos_x", value = input$video_pos3_x)

    if (!is.null(input$video_pos2_x)) {
      updateSliderInput(session, "video_pos2_x", value = input$video_pos3_x)
    }
  }

  refresh_display(refresh_display() + 1)
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(session$getBookmarkExclude(), "optimize_blobs_x"))
