#--------------------------------------------------------------
# Packages
#--------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(shinyalert)
library(htmlwidgets)
library(sortable)
library(data.table)
library(Rvision)
library(stringr)


#--------------------------------------------------------------
# Global Variables
#--------------------------------------------------------------
app_folder <- getwd()


#--------------------------------------------------------------
# User Interface
#--------------------------------------------------------------
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
          style = "width: min(100vh, calc(100% - 410px));
            float: left;
            margin-top: 20px;
            margin-left: calc((calc(100% - 410px) -
              min(100vh, calc(100% - 410px))) / 2)",
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
            load_module(app_folder, "video", "UI"),
            load_module(app_folder, "background", "UI"),
            load_module(app_folder, "mask", "UI"),
            load_module(app_folder, "segmentation", "UI"),
            load_module(app_folder, "blob", "UI"),
            load_module(app_folder, "track", "UI")
          ),
          verticalTabsetPanel(
            id = "settings",
            contentWidth = 11,
            menuSide = "right",
            load_module(app_folder, "controls", "UI")
          )
        )
      ),
    )
  )
}


#--------------------------------------------------------------
# Application server
#--------------------------------------------------------------
server <- function(input, output, session) {
  load_module(app_folder, "video", "Server")
  load_module(app_folder, "background", "Server")
  load_module(app_folder, "mask", "Server")
  load_module(app_folder, "segmentation", "Server")
  load_module(app_folder, "blob", "Server")
  load_module(app_folder, "track", "Server")
  load_module(app_folder, "controls", "Server")

  session$onSessionEnded(function() {

  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
