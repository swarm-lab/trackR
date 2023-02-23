library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(sortable)
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
            moduleUI("trackR/video"),
            moduleUI("trackR/background"),
            moduleUI("trackR/mask"),
            moduleUI("trackR/segmentation"),
            moduleUI("trackR/blob"),
            moduleUI("trackR/track")
          ),

          verticalTabsetPanel(
            id = "settings",
            contentWidth = 11,
            menuSide = "right",
            moduleUI("trackR/controls")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  moduleSVR("trackR/video")

  moduleSVR("trackR/background")

  moduleSVR("trackR/mask")

  moduleSVR("trackR/segmentation")

  moduleSVR("trackR/blob")

  moduleSVR("trackR/track")

  moduleSVR("trackR/controls")

  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
