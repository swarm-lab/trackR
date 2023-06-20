library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(sortable)
library(data.table)
library(Rvision)

newDisplay("trackR")

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "trackR.css")
      ),

      useShinyjs(),
      extendShinyjs(text = "shinyjs.replace = function(url) {
                              location.replace(url);
                            }", functions = "replace"),

      div(
        style = "width: 100%;",

        div(
          style = "width: calc(100% - 410px); float: left; margin-top: 20px;",
          class = "vrtc-tab-panel-container",
          uiOutput("display")
        ),

        div(
          style = "width: 400px; margin-left: calc(100% - 400px);",

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
      ),



      # fluidRow(
      #   column(
      #     8,
      #     panel(heading = "TEST")
      #   ),
      #
      #   column(
      #     4,
      #
      #     verticalTabsetPanel(
      #       id = "main",
      #       contentWidth = 11,
      #       menuSide = "right",
      #       selected = "1",
      #       moduleUI("trackR/video"),
      #       moduleUI("trackR/background"),
      #       moduleUI("trackR/mask"),
      #       moduleUI("trackR/segmentation"),
      #       moduleUI("trackR/blob"),
      #       moduleUI("trackR/track")
      #     ),
      #
      #     verticalTabsetPanel(
      #       id = "settings",
      #       contentWidth = 11,
      #       menuSide = "right",
      #       moduleUI("trackR/controls")
      #     )
      #   )
      # )
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
