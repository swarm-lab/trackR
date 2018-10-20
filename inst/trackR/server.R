shinyServer(function(input, output, session) {

  theActive <- reactiveVal("video")
  thePath <- reactiveVal()
  theFile <- reactiveVal()
  theVideo <- reactiveVal()
  theImage <- reactiveVal()
  theBackground <- reactiveVal()
  theMask <- reactiveVal(NULL)
  theBlobs <- reactiveVal()
  theBlobSizes <- {}
  theSuccess <- reactiveVal(FALSE)

  newDisplay("trackR")

  source("SERVER/video.R", local = TRUE)

  source("SERVER/background.R", local = TRUE)

  source("SERVER/mask.R", local = TRUE)

  source("SERVER/blob.R", local = TRUE)

  source("SERVER/tracking.R", local = TRUE)

  output$videoSlider <- renderUI({
    if (isVideo(theVideo()) & !is.null(input$rangePos)) {
      sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                  max = diff(input$rangePos) + 1, step = 1)
    } else {
      sliderInput("videoPos", "Frame", width = "100%", value = 1, min = 1,
                  max = 1, step = 1)
    }
  })

  # Clean up
  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
})
