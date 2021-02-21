library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)

Rvision::newDisplay("videoFixer")

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "videoFixer.css")
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
            moduleUI("videoFixer/video"),
            moduleUI("videoFixer/fix")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  moduleSVR("videoFixer/video")
  moduleSVR("videoFixer/fix")
  disable(selector = "a[data-value=2]")

  session$onSessionEnded(function() {
    Rvision::destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
