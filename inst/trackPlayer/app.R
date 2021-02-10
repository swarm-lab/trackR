library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)

Rvision::newDisplay("trackR")
cbPalette <- c("#FFBF80", "#FF8000", "#FFFF99", "#FFFF33", "#B2FF8C", "#33FF00",
               "#A6EDFF", "#1AB2FF", "#CCBFFF", "#664CFF", "#FF99BF", "#E61A33")

jscode <- "shinyjs.init = function() {
  $(document).keydown(function(e) {
    // alert('Key pressed: ' + e.which);

    if(e.which == 32) {
      e.preventDefault();
      $('#playPause_x')[0].click();
    };

    if(e.which == 37) {
      e.preventDefault();
      $('#minusFrame_x')[0].click();
    };

    if(e.which == 39) {
      e.preventDefault();
      $('#plusFrame_x')[0].click();
    };

    if(e.which == 40) {
      e.preventDefault();
      $('#minusSec_x')[0].click();
    };

    if(e.which == 38) {
      e.preventDefault();
      $('#plusSec_x')[0].click();
    };
  });
}"

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),

      useShinyjs(),
      extendShinyjs(text = jscode, functions = "onKeypress"),

      fluidRow(
        column(
          12,

          verticalTabsetPanel(
            id = "main",
            contentWidth = 11,
            menuSide = "right",
            selected = "1",
            moduleUI("trackPlayer/data"),
            moduleUI("trackPlayer/play")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  moduleSVR("trackPlayer/data")
  moduleSVR("trackPlayer/play")
  disable(selector = "a[data-value=2]")

  session$onSessionEnded(function() {
    Rvision::destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
