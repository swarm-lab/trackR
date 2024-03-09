#--------------------------------------------------------------
# Server Variables
#--------------------------------------------------------------
volumes <- c(Home = fs::path_home(), getVolumes()())
the_video <- NULL
the_image <- NULL
range_mem <- c(NA, NA)
frame_mem <- NA
tmp_dir <- tempdir()


#--------------------------------------------------------------
# Reactive Variables
#--------------------------------------------------------------
the_video_path <- reactiveVal()
the_frame <- reactiveVal()
default_root <- reactiveVal()
default_path <- reactiveVal("")
refresh_video <- reactiveVal(0)
refresh_display <- reactiveVal(0)
print_display <- reactiveVal(0)


#--------------------------------------------------------------
# Status Outputs
#--------------------------------------------------------------
output$video_status <- renderUI({
  if (refresh_display() > -1 & !isVideoStack(the_video) &
    length(input$sortedVideos) == 0) {
    toggleTabs(2:6, "OFF")
    p("Video missing (and required).", class = "bad")
  } else if (!isVideoStack(the_video)) {
    toggleTabs(2:6, "OFF")
    p("Incompatible videos.", class = "bad")
  } else {
    toggleTabs(2, "ON")
  }
})


#--------------------------------------------------------------
# UI Outputs
#--------------------------------------------------------------
output$video_list <- renderUI({
  if (!is.null(the_video_path())) {
    tags$div(
      class = "panel panel-default",
      tags$div(
        class = "panel-heading",
        icon("video"),
        "Selected videos (drag to reorder)"
      ),
      rank_list(
        text = NULL,
        labels = the_video_path()$name,
        input_id = "sortedVideos",
        options = sortable_options(
          group = list(group = "group")
        )
      ),
      tags$div(
        class = "panel-heading",
        icon("trash"),
        "Remove video from list (drag & drop below)"
      ),
      tags$div(
        class = "panel-body",
        style = "background: repeating-linear-gradient(
                -45deg,
                #f5f5f5,
                #f5f5f5 10px,
                #999999 10px,
                #999999 20px
              );",
        id = "bin"
      ),
      sortable_js(
        "bin",
        options = sortable_options(
          group = list(
            group = "group",
            put = TRUE,
            pull = TRUE
          ),
          onAdd = JS("function (evt) { this.el.removeChild(evt.item); }")
        )
      )
    )
  }
})

output$range_slider <- renderUI({
  if (refresh_video() > 0 & isVideoStack(the_video)) {
    sliderInput("range_pos_x", "Video range",
      width = "100%", min = 1,
      max = nframes(the_video),
      value = c(1, nframes(the_video)), step = 1
    )
  }
})

output$video_slider <- renderUI({
  if (refresh_video() > 0 & !is.null(input$range_pos_x) &
    isVideoStack(the_video)) {
    if (any(is.na(range_mem))) {
      range_mem <<- input$range_pos_x
    }

    test <- range_mem != input$range_pos_x
    range_mem <<- input$range_pos_x

    if (test[2] & !test[1]) {
      sliderInput("video_pos_x", "Frame",
        width = "100%", step = 1,
        value = input$range_pos_x[2],
        min = input$range_pos_x[1],
        max = input$range_pos_x[2]
      )
    } else {
      sliderInput("video_pos_x", "Frame",
        width = "100%", step = 1,
        value = input$range_pos_x[1],
        min = input$range_pos_x[1],
        max = input$range_pos_x[2]
      )
    }
  }
})


#--------------------------------------------------------------
# Events
#--------------------------------------------------------------
observeEvent(input$main, {
  if (input$main == "1") {
    refresh_display(refresh_display() + 1)
  }
})

shinyFileChoose(input, "video_file_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$video_file_x, {
  path <- parseFilePaths(volumes, input$video_file_x)
  if (nrow(path) > 0) {
    if (is.data.frame(the_video_path())) {
      ord <- match(input$sortedVideos, the_video_path()$name)
      tmp <- rbind(the_video_path()[ord, ], path)
      the_video_path(tmp[!duplicated(tmp), ])
    } else {
      the_video_path(path)
    }
  }
})

observeEvent(input$sortedVideos, {
  l <- nrow(the_video_path())

  ix <- which.max(
    sapply(
      stringr::str_locate_all(the_video_path()$datapath[l], volumes),
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

  if (length(volume) > 0) {
    dir <- dirname(the_video_path()$datapath[l])
    default_root(names(volumes)[ix])
    default_path(gsub(volume, "", dir))
  }
})

observeEvent(input$sortedVideos, {
  if (isVideoStack(the_video)) {
    lapply(the_video, function(x) x$release())

    if (!all(sapply(the_video, function(x) x$isOpened()))) {
      the_video <<- NULL
    } else {
      cat("An error occured while trying to release the video stack.\n")
    }
  }

  ord <- match(input$sortedVideos, the_video_path()$name)
  to_check <- tryCatch(videoStack(unlist(the_video_path()$datapath[ord])),
    error = function(e) NA
  )

  if (isVideoStack(to_check)) {
    if (nframes(to_check) > 0) {
      the_video <<- to_check
      the_image <<- readFrame(the_video, 1)

      if (isImage(the_background)) {
        if (!all(dim(the_background) == dim(the_image))) {
          the_background <<- NULL
        }
      }

      if (isImage(the_mask)) {
        if (!all(dim(the_mask) == dim(the_image))) {
          the_mask <<- NULL
        }
      }
    } else {
      the_video <<- NULL
      the_image <<- NULL
    }
  } else {
    the_image <<- NULL
  }

  refresh_video(refresh_video() + 1)
  refresh_display(refresh_display() + 1)
})

observeEvent(the_frame(), {
  if (!is.null(the_frame())) {
    readFrame(the_video, the_frame(), the_image)
    refresh_display(refresh_display() + 1)
  }
})


#--------------------------------------------------------------
# Display
#--------------------------------------------------------------
observeEvent(refresh_display(), {
  if (input$main == "1") {
    if (isImage(the_image)) {
      suppressMessages(
        write.Image(the_image, paste0(tmp_dir, "/display.jpg"), TRUE)
      )
    } else {
      suppressMessages(
        write.Image(zeros(1080, 1920, 3), paste0(tmp_dir, "/display.jpg"), TRUE)
      )
    }

    print_display(print_display() + 1)
  }
})

output$display <- renderUI({
  if (collect_ghost() | collect_mask() | collect_scale()) {
    imageOutput("displayImg",
      height = "auto",
      click = "plot_click",
      dblclick = "plot_dblclick"
    )
  } else {
    imageOutput("displayImg",
      height = "auto"
    )
  }
})

output$displayImg <- renderImage(
  {
    if (print_display() > 0) {
      iw <- ncol(the_image)
      ih <- nrow(the_image)
      ww <- session$clientData[["output_displayImg_width"]] - 20
      wh <- (session$clientData[["output_displayImg_width"]] - 20) * ih / iw

      if (collect_ghost() | collect_mask() | collect_scale()) {
        plot.new()
        op <- par(mar = rep(0, 4))
        xlim <- c(1, iw)
        ylim <- c(1, ih)
        plot(NA,
          xlim = xlim + c(-0.5, 0.5), ylim = ylim + c(-0.5, 0.5), asp = 1,
          xaxt = "n", yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i"
        )
        par(op)
        cm <- shiny:::getCoordmap(NULL, iw, ih)
        dev.off()
        list(
          src = paste0(tmp_dir, "/display.jpg"),
          width = ww,
          height = wh,
          coordmap = cm
        )
      } else {
        list(
          src = paste0(tmp_dir, "/display.jpg"),
          width = ww,
          height = wh
        )
      }
    }
  },
  deleteFile = FALSE
)

observeEvent(input$video_pos_x, {
  the_frame(input$video_pos_x)
  frame_mem <<- input$video_pos_x

  if (input$main == "1") {
    if (!is.null(input$video_pos2_x)) {
      updateSliderInput(session, "video_pos2_x", value = input$video_pos_x)
    }

    if (!is.null(input$video_pos3_x)) {
      updateSliderInput(session, "video_pos3_x", value = input$video_pos_x)
    }
  }
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(
  session$getBookmarkExclude(), "refresh_video", "refresh_display",
  "video_file_x", "video_file_x-modal"
))

onBookmark(function(state) {
  state$values$the_video_path <- the_video_path()
})

onRestore(function(state) {
  the_video_path(as.data.frame(state$values$the_video_path))
})
