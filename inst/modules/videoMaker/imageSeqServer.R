volumes <- c(Home = fs::path_home(), getVolumes()())

theSequencePath <- reactiveVal()
theSequence <- reactiveVal()
theImage <- NULL
theTrackVideoPath <- reactiveVal()
defaultRoot <- reactiveVal(NULL)
defaultPath <- reactiveVal("")
redraw <- reactiveVal(0)

# Toggle UI on and off during long operations
toggleAll <- function(state = "OFF") {
  input_list <- reactiveValuesToList(input)
  to_toggle <- grepl("_x", names(input_list))
  input_list <- input_list[to_toggle]

  for (name in names(input_list)) {
    if (state == "OFF") {
      shinyjs::disable(name)
    } else {
      shinyjs::enable(name)
    }
  }
}

# Select video
shinyDirChoose(input, "sequenceFiles_x", roots = volumes, session = session,
               defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$sequenceFiles_x, {
  path <- parseDirPath(volumes, input$sequenceFiles_x)
  if (length(path) > 0) {
    theSequencePath(path)
  }
})

observeEvent(theSequencePath(), {
  ix <- which.max(
    sapply(
      stringr::str_locate_all(theSequencePath(), volumes),
      function(l) {
        if (nrow(l) > 0) {
          diff(l[1, ])
        } else {
          NA
        }
      })
  )
  volume <- volumes[ix]
  defaultRoot(names(volumes)[ix])
  defaultPath(gsub(fs::path(volume), "", theSequencePath()))
})

observeEvent(theSequencePath(), {
  img_files <- list.files(theSequencePath(), ".png|.jpg|.jpeg|.tif|.tiff|.bmp",
                          ignore.case = TRUE, full.names = TRUE)
  toCheck <- tryCatch(image(img_files[1]),
                      error = function(e) NA)

  if (isImage(toCheck)) {
    theImage <<- toCheck
    theSequence(img_files)
  }
})

output$seqStatus <- renderUI({
  if (length(theSequence()) < 1) {
    p("Image sequence missing (and required).", class = "bad")
  }
})

# Controls
output$displaySlider <- renderUI({
  if (length(theSequence()) > 0) {
    sliderInput("displaySize_x", "Output rescaling", width = "100%", value = 1,
                min = 0.1, max = 1, step = 0.1)
  }
})

output$rangeSlider <- renderUI({
  if (length(theSequence()) > 0) {
    tagList(
      hr(),
      sliderInput("rangePos_x", "Select sequence range", width = "100%", min = 1,
                  max = length(theSequence()),
                  value = c(1, length(theSequence())), step = 1)
    )
  }
})

rangeMem <- c(NA, NA)
output$seqSlider <- renderUI({
  if (length(theSequence()) > 0 & !is.null(input$rangePos_x)) {
    if (any(is.na(rangeMem))) {
      rangeMem <<- input$rangePos_x
    }

    test <- rangeMem != input$rangePos_x
    rangeMem <<- input$rangePos_x

    if (test[2] & !test[1]) {
      tagList(
        sliderInput("seqPos_x", "Frame", width = "100%",
                    value = diff(input$rangePos_x) + 1,
                    min = 1, max = diff(input$rangePos_x) + 1, step = 1),
        hr()
      )
    } else {
      tagList(
        sliderInput("seqPos_x", "Frame", width = "100%",
                    value = 1, min = 1, max = diff(input$rangePos_x) + 1, step = 1),
        hr()
      )
    }
  }
})

# Display video
observeEvent(input$seqPos_x, {
  if (!is.null(input$seqPos_x) & !is.null(input$displaySize_x) & !is.null(input$rangePos_x)){
    theImage <<- image(theSequence()[input$seqPos_x + input$rangePos_x[1] - 1])
    redraw(redraw() + 1)
  }
})

observeEvent(input$displaySize_x, {
  if (!is.null(input$seqPos_x) & !is.null(input$displaySize_x) & !is.null(input$rangePos_x)){
    redraw(redraw() + 1)
  }
})

observeEvent(input$rangePos_x, {
  if (!is.null(input$seqPos_x) & !is.null(input$displaySize_x) & !is.null(input$rangePos_x)){
    theImage <<- image(theSequence()[input$seqPos_x + input$rangePos_x[1] - 1])
    redraw(redraw() + 1)
  }
})

observeEvent(redraw(), {
  if (length(theSequence()) > 0) {
    display(theImage, "videoMaker", 5,
            nrow(theImage) * input$displaySize_x,
            ncol(theImage) * input$displaySize_x)
  } else {
    display(zeros(480, 640), "videoMaker", 5, 480, 640)
  }
})

# Export video
output$exportButton <- renderUI({
  if (length(theSequence()) > 0 & !is.null(input$compression_x)) {
    if (input$compression_x == TRUE) {
      tagList(
        hr(),
        shinySaveButton("exportVideo_x", "Export video", "Save video as...",
                        filetype = "mp4",
                        class = "fullWidth")
      )
    } else {
      tagList(
        hr(),
        shinySaveButton("exportVideo_x", "Export video", "Save video as...",
                        filetype = "mkv",
                        class = "fullWidth")
      )
    }
  }
})

output$fpsLabel <- renderUI({
  if (length(theSequence()) > 0) {
    HTML("<b>FPS: </b>")
  }
})

output$fpsSelector <- renderUI({
  if (length(theSequence()) > 0) {
    numericInput("fps_x", NULL, 25, 1, NA, 1, "100%")
  }
})

output$compressionSelector <- renderUI({
  if (length(theSequence()) > 0) {
    materialSwitch(inputId = "compression_x", label = "Compression",
                   value = TRUE, status = "success")
  }
})

shinyFileSave(input, "exportVideo_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$exportVideo_x, {
  path <- parseSavePath(volumes, input$exportVideo_x)
  theTrackVideoPath(path$datapath)
})

observeEvent(theTrackVideoPath(), {
  if (length(theTrackVideoPath()) > 0 & length(theSequence()) > 0) {
    toggleAll("OFF")

    showNotification("Exporting video.", id = "exporting", duration = NULL)

    range_pos <- input$rangePos_x
    n <- diff(range_pos) + 1

    if (input$compression_x == TRUE) {
      vw <- videoWriter(theTrackVideoPath(),
                        fourcc = "avc1",
                        fps = input$fps_x,
                        height = nrow(theImage) * input$displaySize_x,
                        width = ncol(theImage) * input$displaySize_x)
    } else {
      vw <- videoWriter(theTrackVideoPath(),
                        fourcc = "ffv1",
                        fps = input$fps_x,
                        height = nrow(theImage) * input$displaySize_x,
                        width = ncol(theImage) * input$displaySize_x)
    }

    pb <- Progress$new()
    pb$set(message = "Processing: ", value = 0, detail = "0%")
    old_check <- 0
    old_frame <- 1
    old_time <- Sys.time()

    for (i in 1:n) {
      pos <- i + range_pos[1] - 1
      img <- image(theSequence()[i])

      if (input$displaySize_x < 1) {
        img <- resize(img, fx = input$displaySize_x, fy = input$displaySize_x)
      }

      writeFrame(vw, img)

      if ((i %% 25) == 0)
        invisible(gc())

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

    release(vw)

    pb$close()
    removeNotification(id = "exporting")
    toggleAll("ON")
  }
})
