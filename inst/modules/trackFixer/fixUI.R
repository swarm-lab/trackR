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

  tags$table(
    tags$tr(
      tags$td(actionButton("reassignTrack_x", "Reassign [q]", width = "100%"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(actionButton("removeTrack_x", "Remove [w]", width = "100%"),
              style = "width: 49%;")
    ),

    tags$tr(),

    tags$tr(
      tags$td(actionButton("swapTrack_x", "Swap IDs [e]", width = "100%"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(actionButton("mergeTrack_x", "Merge IDs [r]", width = "100%"),
              style = "width: 49%;")
    ),

    tags$tr(),

    tags$tr(
      tags$td(actionButton("revertChanges_x", "Undo [a]", width = "100%"),
              style = "width: 49%;"),
      tags$td(),
      tags$td(actionButton("saveChanges_x", "Save [s]", width = "100%"),
              style = "width: 49%;")
    ),

    class = "settingsTable"
  ),

  hr(),

  tableOutput("trackStats"),

  hr(),

  htmlOutput("displaySlider")
)
