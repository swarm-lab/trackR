bsCollapsePanel(
  title = actionLink("toggleBlob", "Blob detection"),
  value = "blobPanel",
  htmlOutput("backgroundStatusBlob", container = p, class = "good"),
  sliderInput("blobBlur", "Smoothing factor:", min = 0, max = 10,
              value = 3, width = "100%"),
  sliderInput("blobThreshold", "Detection threshold:", min = 1, max = 255,
              value = 50, width = "100%"),
  sliderInput("blobSize", "Size range:", min = 1, max = 10000, value = c(1, 10000),
              width = "100%"),
  actionButton("recordBlobs", "Record blobs", width = "100%")
)
