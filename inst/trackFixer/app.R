library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(Rvision)

newDisplay("trackFixer")
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

    if(e.which == 81) {
      e.preventDefault();
      $('#reassignTrack_x')[0].click();
    };

    if(e.which == 87) {
      e.preventDefault();
      $('#removeTrack_x')[0].click();
    };

    if(e.which == 69) {
      e.preventDefault();
      $('#swapTrack_x')[0].click();
    };

    if(e.which == 82) {
      e.preventDefault();
      $('#mergeTrack_x')[0].click();
    };

    if(e.which == 65) {
      e.preventDefault();
      $('#revertChanges_x')[0].click();
    };

    if(e.which == 83) {
      e.preventDefault();
      $('#saveChanges_x')[0].click();
    };
  });
}"

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "trackFixer.css")
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
            moduleUI("trackFixer/data"),
            moduleUI("trackFixer/fix")
          )
        )
      )
    )
  )
}

server <- function(input, output, session) {
  moduleSVR("trackFixer/data")
  moduleSVR("trackFixer/fix")
  disable(selector = "a[data-value=2]")

  session$onSessionEnded(function() {
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
