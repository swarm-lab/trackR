verticalTabPanel(
  title = "2",
  id = "fixPanel",
  box_height = "100%",
  p("Fixing module", class = "module-title"),

  hr(),

  htmlOutput("videoStatus2"),
  htmlOutput("trackStatus2"),

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

  tags$table(style = "width: 100%; margin-bottom: 10px;",
             tags$tr(
               actionButton("reassignTrack_x", "Reassign [q]", class = "halfWidth"),
               actionButton("removeTrack_x", "Remove [w]", class = "halfWidth")
             ),
             tags$tr(
               actionButton("swapTrack_x", "Swap IDs [e]", class = "halfWidth"),
               actionButton("mergeTrack_x", "Merge IDs [r]", class = "halfWidth")
             )
  ),

  tags$table(style = "width: 100%; margin-bottom: 10px;",
             tags$tr(
               actionButton("revertChanges_x", "Undo [a]", class = "halfWidth"),
               actionButton("saveChanges_x", "Save [s]", class = "halfWidth")
             )
  ),

  hr(),

  tableOutput("trackStats"),

  hr(),

  htmlOutput("displaySlider")
)
