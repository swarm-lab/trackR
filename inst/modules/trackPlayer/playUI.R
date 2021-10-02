verticalTabPanel(
  title = "2",
  id = "fixPanel",
  box_height = "100%",
  p("Play module", class = "module-title"),

  hr(),

  htmlOutput("rangeSlider"),

  hr(),

  tags$table(style = "width: 100%; margin-bottom: 10px;",
             tags$tr(
               actionButton("playPause_x", "Play/Pause [space]", width = "100%")
             ),
             tags$tr(
               actionButton("minusSec_x", "<< [↓]", class = "quarterWidth"),
               actionButton("minusFrame_x", "< [←]", class = "quarterWidth"),
               actionButton("plusFrame_x", "[→] >", class = "quarterWidth"),
               actionButton("plusSec_x", "[↑] >>", class = "quarterWidth")
             )
  ),

  htmlOutput("videoSlider"),

  hr(),

  htmlOutput("displaySlider"),

  hr(),

  tags$table(
    tags$tr(
      tags$td(actionButton("title_x", "Set title", width = "100%"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(actionButton("subtitle_x", "Set subtitle", width = "100%"),
              style = "width: 49%;")
    ),

    tags$tr(),

    tags$tr(
      tags$td(actionButton("authors_x", "Set author list", width = "100%"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(actionButton("timestamp_x", "Set timestamp", width = "100%"),
              style = "width: 49%;")
    ),

    class = "settingsTable"
  ),

  hr(),

  shinySaveButton("exportVideo_x", "Export video", "Save video as...",
                  filetype = "mp4",
                  class = "fullWidth"),

  p()
)
