#--------------------------------------------------------------
# Server Variables
#--------------------------------------------------------------
the_background <- NULL
ghost_coords <- NULL


#--------------------------------------------------------------
# Reactive Variables
#--------------------------------------------------------------
the_background_path <- reactiveVal()
refresh_background <- reactiveVal(0)
collect_ghost <- reactiveVal(0)
stop_ghost_collection <- reactiveVal(0)


#--------------------------------------------------------------
# Status Outputs
#--------------------------------------------------------------
output$backgroundStatus <- renderUI({
  if (refresh_display() > -1 & !isImage(the_background)) {
    toggleTabs(3:6, "OFF")
    p("Background missing (and required).", class = "bad")
  } else {
    toggleTabs(3, "ON")
  }
})


#--------------------------------------------------------------
# Events
#--------------------------------------------------------------
observeEvent(input$main, {
  if (input$main == "2") {
    refresh_display(refresh_display() + 1)
  }
})

shinyFileChoose(input, "backgroundFile_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$backgroundFile_x, {
  path <- parseFilePaths(volumes, input$backgroundFile_x)
  if (nrow(path) > 0) {
    the_background_path(path$datapath)
    refresh_background(refresh_background() + 1)
  }
})

observeEvent(refresh_background(), {
  if (refresh_background() > 0) {
    to_check <- tryCatch(image(the_background_path()), error = function(e) NA)

    if (isImage(to_check)) {
      if (colorspace(to_check) != "BGR") {
        changeColorSpace(to_check, "BGR", "self")
      }

      if (!all(dim(to_check) == dim(the_image))) {
        shinyalert("Error:",
          "The video and background do not have the same dimensions.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        the_background <<- NULL
      } else {
        the_background <<- cloneImage(to_check)
      }

      ix <- which.max(
        sapply(
          str_locate_all(the_background_path(), volumes),
          function(l) {
            if (nrow(l) > 0) {
              diff(l[1, ])
            } else {
              NA
            }
          }
        )
      )

      volume <- volumes[ix]
      dir <- dirname(the_background_path())
      default_root(names(volumes)[ix])
      default_path(gsub(volume, "", dir))

      refresh_display(refresh_display() + 1)
    }
  }
})

observeEvent(input$computeBackground_x, {
  if (isVideoStack(the_video)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")
    the_background <<- backgrounder(the_video,
      n = input$backroundImages_x,
      method = input$backgroundType_x,
      start = input$range_pos_x[1],
      end = input$range_pos_x[2]
    )
    changeBitDepth(the_background, "8U", target = "self")
    toggleInputs("ON")
    toggleTabs(1:2, "ON")
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(refresh_display(), {
  if (input$main == "2") {
    if (isImage(the_background)) {
      to_display <- cloneImage(the_background)
      sc <- max(dim(to_display) / 720)
      r <- 0.01 * min(nrow(to_display), ncol(to_display))

      if (collect_ghost() > 0) {
        if (nrow(ghost_coords) > 1) {
          drawPolyline(to_display, ghost_coords,
            closed = TRUE, color = "white",
            thickness = max(1, 1.5 * sc)
          )
        }

        if (nrow(ghost_coords) > 0) {
          drawCircle(to_display,
            x = ghost_coords[, 1], y = ghost_coords[, 2],
            radius = r * 1.5, thickness = -1, color = "white"
          )
          drawCircle(to_display,
            x = ghost_coords[, 1], y = ghost_coords[, 2],
            radius = r, thickness = -1, color = "red"
          )
        }
      }

      suppressMessages(
        write.Image(to_display, paste0(tmp_dir, "/display.jpg"), TRUE)
      )
    } else if (isImage(the_image)) {
      suppressMessages(write.Image(
        zeros(nrow(the_image), ncol(the_image), 3),
        paste0(tmp_dir, "/display.jpg"), TRUE
      ))
    } else {
      suppressMessages(write.Image(
        zeros(1080, 1920, 3),
        paste0(tmp_dir, "/display.jpg"), TRUE
      ))
    }

    print_display(print_display() + 1)
  }
})

observeEvent(input$ghostButton_x, {
  if (isImage(the_background)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")

    showNotification("Click to draw a polygon around the object to remove from
                     the image. Double-click to stop.",
      id = "ghost_notif",
      duration = NULL, type = "message"
    )

    collect_ghost(1)
  }
})

observeEvent(input$plot_click, {
  if (collect_ghost() > 0) {
    clck <- input$plot_click$coords_img
    clck$y <- -clck$y + nrow(the_background) + 1
    ghost_coords <<- rbind(ghost_coords, unlist(clck))
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$plot_dblclick, {
  if (collect_ghost() > 0) {
    stop_ghost_collection(stop_ghost_collection() + 1)
  }
})

observeEvent(stop_ghost_collection(), {
  if (collect_ghost() > 0) {
    if (nrow(ghost_coords) > 0) {
      roi <- zeros(nrow(the_background), ncol(the_background), 1)
      fillPoly(roi, ghost_coords, "white")
      inpaint(the_background, roi, method = "Telea", target = "self")
    }

    removeNotification(id = "ghost_notif")
    toggleInputs("ON")
    toggleTabs(1:2, "ON")
    collect_ghost(0)
    ghost_coords <<- NULL
    refresh_display(refresh_display() + 1)
  }
})

shinyFileSave(input, "save_background_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$save_background_x, {
  path <- parseSavePath(volumes, input$save_background_x)

  if (isImage(the_background) & nrow(path) > 0) {
    write.Image(the_background, path$datapath, TRUE)
    the_background_path(path$datapath)
  }
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(
  session$getBookmarkExclude(), "backgroundFile_x", "refresh_background",
  "computeBackground_x", "ghostButton_x", "save_background_x"
))

onBookmark(function(state) {
  state$values$the_background_path <- the_background_path()
})

onRestore(function(state) {
  the_background_path(state$values$the_background_path[[1]])
  refresh_background(refresh_background() + 1)
})
