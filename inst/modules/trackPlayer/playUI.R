verticalTabPanel(
  title = "2",
  id = "fixPanel",
  box_height = "100%",
  p("Fixing module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus2"),
  htmlOutput("trackStatus2"),

  htmlOutput("videoSlider"),

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

  hr(),

  htmlOutput("displaySlider"),

  hr(),

  shinySaveButton("exportVideo_x", "Export video", "Save video as...",
                  filetype = "mp4",
                  class = "fullWidth"),

  p()
)
