library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(data.table)
library(Rvision)

newDisplay("trackR")
jscode <- "shinyjs.replace = function(url) { location.replace(url); }"

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "trackR.css")
      ),

      useShinyjs(),
      extendShinyjs(text = jscode, functions = "replace"),

      fluidRow(
        column(
          12,

          verticalTabsetPanel(
            id = "main",
            contentWidth = 11,
            menuSide = "right",
            selected = "1",
            moduleUI("trackR_lab/video"),
            moduleUI("trackR_lab/background"),
            moduleUI("trackR_lab/mask"),
            moduleUI("trackR_lab/segmentation"),
            moduleUI("trackR_lab/blob"),
            moduleUI("trackR_lab/track")
          ),

          verticalTabsetPanel(
            id = "settings",
            contentWidth = 11,
            menuSide = "right",
            moduleUI("trackR_lab/controls")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  moduleSVR("trackR_lab/video")

  moduleSVR("trackR_lab/background")

  moduleSVR("trackR_lab/mask")

  moduleSVR("trackR_lab/segmentation")

  moduleSVR("trackR_lab/blob")

  moduleSVR("trackR_lab/track")

  moduleSVR("trackR_lab/controls")

  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
