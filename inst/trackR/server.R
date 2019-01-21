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

  settings <- list(path = NA, file = NA, quality = NA, size = NA, range = NA,
                   background = NA, mask = NA, smoothing = NA, threshold = NA,
                   blob_size = NA, lookback = NA, max_dist = NA)

  newDisplay("trackR")

  source("SERVER/video.R", local = TRUE)

  source("SERVER/background.R", local = TRUE)

  source("SERVER/mask.R", local = TRUE)

  source("SERVER/blob.R", local = TRUE)

  source("SERVER/tracking.R", local = TRUE)

  source("SERVER/control.R", local = TRUE)

  # Clean up
  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
})

