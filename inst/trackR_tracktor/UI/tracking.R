bsCollapsePanel(
  title = actionLink("toggleTracking", "Tracking"),
  value = "trackingPanel",
  htmlOutput("videoStatusTracking", container = p, class = "good"),
  sliderInput("lookBack", "Look back (frames):", min = 1, max = 150,
              value = 5, width = "100%"),
  sliderInput("maxDist", "Maximum distance (pixels):", min = 1,
              max = 200, value = 30, width = "100%"),
  checkboxInput("showTracks", "Display tracks (slower)", FALSE),
  actionButton("computeTracks", "Start tracking"),
  htmlOutput("trackingStatus")
)
