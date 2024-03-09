#--------------------------------------------------------------
# Server Variables
#--------------------------------------------------------------
the_mask <- NULL
mask_coords <- NULL


#--------------------------------------------------------------
# Reactive Variables
#--------------------------------------------------------------
the_mask_path <- reactiveVal()
refresh_mask <- reactiveVal(0)
collect_mask <- reactiveVal(0)
stop_mask_collection <- reactiveVal(0)


#--------------------------------------------------------------
# Status Outputs
#--------------------------------------------------------------
observeEvent(print_display(), {
  if (!isImage(the_mask)) {
    toggleTabs(4:6, "OFF")
  } else {
    toggleTabs(4, "ON")
  }
})


#--------------------------------------------------------------
# Events
#--------------------------------------------------------------
observeEvent(input$main, {
  if (input$main == "3") {
    refresh_display(refresh_display() + 1)
  }
})

shinyFileChoose(input, "mask_file_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$mask_file_x, {
  path <- parseFilePaths(volumes, input$mask_file_x)
  if (nrow(path) > 0) {
    the_mask_path(path$datapath)
    refresh_mask(refresh_mask() + 1)
  }
})

observeEvent(refresh_mask(), {
  if (refresh_mask() > 0) {
    to_check <- tryCatch(image(the_mask_path()), error = function(e) NA)

    if (isImage(to_check)) {
      if (colorspace(to_check) != "BGR") {
        changeColorSpace(to_check, "BGR", "self")
      }

      if (!all(dim(to_check) == dim(the_image))) {
        shinyalert("Error:",
          "The video and mask do not have the same dimensions.",
          type = "error", animation = FALSE,
          closeOnClickOutside = TRUE
        )
        the_mask <<- NULL
      } else {
        the_mask <<- cloneImage(to_check)
      }

      ix <- which.max(
        sapply(
          str_locate_all(the_mask_path(), volumes),
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
      dir <- dirname(the_mask_path())
      default_root(names(volumes)[ix])
      default_path(gsub(volume, "", dir))

      refresh_display(refresh_display() + 1)
    }
  }
})

observeEvent(refresh_display(), {
  if (input$main == "3") {
    if (!isImage(the_mask) & isImage(the_image)) {
      the_mask <<- ones(nrow(the_image), ncol(the_image), 3)
    }

    if (isImage(the_mask)) {
      if (isImage(the_image)) {
        to_display <- ones(nrow(the_image), ncol(the_image), 3)
      } else {
        to_display <- zeros(nrow(the_mask), ncol(the_mask), 3)
      }

      setTo(
        to_display,
        changeColorSpace(the_mask, "GRAY"),
        "green",
        target = "self"
      )
      setTo(
        to_display,
        invert(changeColorSpace(the_mask * 255, "GRAY")),
        "red",
        target = "self"
      )

      if (isImage(the_image)) {
        addWeighted(to_display, the_image, c(0.25, 0.75), target = to_display)
      } else {
        addWeighted(to_display, zeros(nrow(the_mask), ncol(the_mask), 3),
          c(0.25, 0.75),
          target = to_display
        )
      }

      sc <- max(dim(to_display) / 720)
      r <- 0.01 * min(nrow(to_display), ncol(to_display))

      setTo(
        to_display,
        morph(
          canny(the_mask, 0, 0), "dilate",
          k_shape = "ellipse",
          k_height = max(1, 0.75 * sc),
          k_width = max(1, 0.75 * sc)
        ),
        "white",
        target = "self"
      )

      ch1 <- extractChannel(the_mask, 1)
      h <- imhist(ch1, nbins = 257, range = c(0, 256))
      valid <- h[(h[, 1] > 0) & (h[, 2] > 0), 1]

      com <- lapply(valid, function(val) {
        cc <- connectedComponents(ch1 == val, stats = FALSE)
        lapply(1:cc$n, function(lab) {
          dt <- distanceTransform(border(cc$labels == lab, 1))
          apply(findNonZero(dt == max(dt)[1]), 2, mean) - 1
        })
      })

      lab <- lapply(com, function(l) {
        lapply(l, function(loc) {
          pget(the_mask, loc[1], loc[2])[1]
        })
      })

      mapply(function(com, lab) {
        mapply(function(com, lab) {
          drawText(to_display, lab,
            com[1] - (floor(log10(lab)) + 1) * 5,
            com[2] - 5 * sc,
            font_scale = 0.5 * sc,
            thickness = max(1, 1.5 * sc),
            color = "white"
          )
        }, com = com, lab = lab)
      }, com = com, lab = lab)

      if (collect_mask() == 1) {
        if (nrow(mask_coords) > 0) {
          drawPolyline(
            to_display,
            mask_coords,
            closed = TRUE,
            color = "white",
            thickness = max(1, 1.5 * sc)
          )
        }
      }

      if (collect_mask() > 0) {
        drawCircle(to_display,
          x = mask_coords[, 1], y = mask_coords[, 2],
          radius = r * 1.5, thickness = -1, color = "white"
        )
        drawCircle(to_display,
          x = mask_coords[, 1], y = mask_coords[, 2],
          radius = r, thickness = -1, color = "red"
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

observeEvent(input$poly_button_x, {
  if (isImage(the_mask)) {
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")
    showNotification("Click to draw the polygonal ROI. Double-click to stop.",
      id = "mask_notif", duration = NULL,
      type = "message"
    )

    collect_mask(1)
  }
})

observeEvent(input$ell_button_x, {
  if (isImage(the_mask)) {
    showNotification("Click to select 5 points along the periphery of the
                     ellipse/circle ROI. Double-click to cancel.",
      id = "mask_notif", duration = NULL,
      type = "message"
    )
    toggleInputs("OFF")
    toggleTabs(1:6, "OFF")
    collect_mask(2)
  }
})

observeEvent(input$plot_click, {
  if (collect_mask() > 0) {
    clck <- input$plot_click$coords_img
    clck$y <- -clck$y + nrow(the_mask) + 1
    mask_coords <<- rbind(mask_coords, unlist(clck))

    if (collect_mask() == 2 & nrow(mask_coords) >= 5) {
      stop_mask_collection(stop_mask_collection() + 1)
    }

    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$plot_dblclick, {
  if (collect_mask() > 0) {
    stop_mask_collection(stop_mask_collection() + 1)
  }
})

observeEvent(stop_mask_collection(), {
  if (collect_mask() > 0) {
    if (collect_mask() == 1) {
      if (nrow(mask_coords) > 2) {
        poly_mask <- zeros(nrow(the_mask), ncol(the_mask), 1)
        fillPoly(poly_mask, mask_coords, color = "white")

        if (input$inc_button_x == "Including") {
          setTo(the_mask, poly_mask, gray(input$roi_x / 255), target = "self")
        } else if (input$inc_button_x == "Excluding") {
          setTo(the_mask, poly_mask, "black", target = "self")
        }
      }
    } else {
      if (nrow(mask_coords) == 5) {
        ell <- optimEllipse(mask_coords[, 1], mask_coords[, 2])
        ell_mask <- zeros(nrow(the_mask), ncol(the_mask), 1)
        drawEllipse(ell_mask, ell[1], ell[2], ell[3] / 2, ell[4] / 2, ell[5],
          color = "white", thickness = -1
        )

        if (input$inc_button_x == "Including") {
          setTo(the_mask, ell_mask, gray(input$roi_x / 255), target = "self")
        } else if (input$inc_button_x == "Excluding") {
          setTo(the_mask, ell_mask, "black", target = "self")
        }
      }
    }

    removeNotification(id = "mask_notif")
    toggleInputs("ON")
    toggleTabs(1:3, "ON")
    collect_mask(0)
    mask_coords <<- NULL
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$inc_button_x, {
  if (input$inc_button_x == "Including") {
    shinyjs::enable("roi_x")
  } else {
    shinyjs::disable("roi_x")
  }
})

observeEvent(input$include_all_x, {
  if (isImage(the_mask)) {
    the_mask <<- ones(nrow(the_mask), ncol(the_mask), 3)
    refresh_display(refresh_display() + 1)
  }
})

observeEvent(input$exclude_all_x, {
  if (isImage(the_mask)) {
    the_mask <<- zeros(nrow(the_mask), ncol(the_mask), 3)
    refresh_display(refresh_display() + 1)
  }
})

shinyFileSave(input, "save_mask_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$save_mask_x, {
  path <- parseSavePath(volumes, input$save_mask_x)

  if (isImage(the_mask) & nrow(path) > 0) {
    write.Image(the_mask, path$datapath, TRUE)
    the_mask_path(path$datapath)
  }
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(
  session$getBookmarkExclude(), "save_mask_x", "mask_file_x",
  "poly_button_x", "inc_button_x", "ell_button_x", "include_all_x",
  "exclude_all_x", "refresh_mask"
))

onBookmark(function(state) {
  state$values$the_mask_path <- the_mask_path()
})

onRestore(function(state) {
  the_mask_path(state$values$the_mask_path[[1]])
  refresh_mask(refresh_mask() + 1)
})
