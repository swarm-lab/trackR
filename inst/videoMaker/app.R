library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(Rvision)

newDisplay("videoMaker")

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "videoMaker.css")
      ),

      useShinyjs(),

      fluidRow(
        column(
          12,

          verticalTabsetPanel(
            id = "main",
            contentWidth = 11,
            menuSide = "right",
            selected = "1",
            moduleUI("videoMaker/imageSeq")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  moduleSVR("videoMaker/imageSeq")

  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
