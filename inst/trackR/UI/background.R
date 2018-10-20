bsCollapsePanel(
  title = actionLink("toggleBackground", "Background"),
  value = "backgroundPanel",
  fluidRow(
    column(width = 6, shinyFilesButton("backgroundFile", "Select background file",
                                       "Please select a background image", FALSE, class = "fullWidth")),
    column(width = 6, actionButton("computeBackground", "...Estimate background", width = "100%"))),
  sliderInput("backroundImages", "Number of images:", min = 0, max = 200,
              value = 10, width = "100%"),
  selectInput("backgroundType", "Type:",
              choices = c("Median" = "median", "Mean" = "mean"), width = "100%"),

  hr(),

  fluidRow(
    column(width = 6, actionButton("ghostButton", "Select ghost for removal", width = "100%")),
    column(width = 6, shinySaveButton("saveBackground", "Save background file", "Save background as...",
                                      filetype = list(picture = c("png", "jpg")),
                                      class = "fullWidth"))),
  htmlOutput("backgroundStatus", container = p, class = "good")
)
