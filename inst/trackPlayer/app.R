library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(colourpicker)

Rvision::newDisplay("trackPlayer")

cbPalette <- c("#FFBF80", "#FF8000", "#FFFF99", "#FFFF33", "#B2FF8C", "#33FF00",
               "#A6EDFF", "#1AB2FF", "#CCBFFF", "#664CFF", "#FF99BF", "#E61A33")

hmsf <- function(f, fps) {
  t <- floor(f / fps)
  paste(formatC(t %/% (60 * 60) %% 24, width = 2, format = "d", flag = "0"),
        formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0"),
        formatC(t %% 60, width = 2, format = "d", flag = "0"),
        formatC(f %% fps, width = 2, format = "d", flag = "0"),
        sep = ":"
  )
}


jscode <- "shinyjs.init = function() {
  $(document).keydown(function(e) {
    // alert('Key pressed: ' + e.which);

    if(e.which == 32 & !($('#shiny-modal').is(':visible'))) {
      e.preventDefault();
      $('#playPause_x')[0].click();
    };

    if(e.which == 37 & !($('#shiny-modal').is(':visible'))) {
      e.preventDefault();
      $('#minusFrame_x')[0].click();
    };

    if(e.which == 39 & !($('#shiny-modal').is(':visible'))) {
      e.preventDefault();
      $('#plusFrame_x')[0].click();
    };

    if(e.which == 40 & !($('#shiny-modal').is(':visible'))) {
      e.preventDefault();
      $('#minusSec_x')[0].click();
    };

    if(e.which == 38 & !($('#shiny-modal').is(':visible'))) {
      e.preventDefault();
      $('#plusSec_x')[0].click();
    };
  });
}"

ui <- function(request) {
  shinyUI(
    fluidPage(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "trackPlayer.css")
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
