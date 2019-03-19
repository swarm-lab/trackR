library(shiny)
library(shinyBS)
library(shinyFiles)
library(shinyjs)
library(Rvision)
library(readr)
library(trackR)

jscode <- "shinyjs.replace = function(url) { location.replace(url); }"

ui <- function(request) {
  shinyUI(fluidPage(

    titlePanel("trackR"),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    useShinyjs(),
    extendShinyjs(text = jscode, functions = "replace"),

    fluidRow(
      column(
        12,

        bsCollapse(
          id = "main",
          multiple = FALSE,
          open = "videoPanel",

          moduleUI("video"),

          moduleUI("backgroundQuanta"),

          moduleUI("mask"),

          moduleUI("blobQuanta"),

          moduleUI("trackingQuanta")
        ),

        moduleUI("controls")
      )
    )

  ))
}

server <- function(input, output, session) {

  moduleServer("controls")

  moduleServer("video")

  moduleServer("backgroundQuanta")

  moduleServer("mask")

  moduleServer("blobQuanta")

  moduleServer("trackingQuanta")

  setBookmarkExclude(bookmarkExclude)

  # Clean up
  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
