#--------------------------------------------------------------
# Toggle UI on and off during long operations
#--------------------------------------------------------------
toggleInputs <- function(state = "OFF") {
  input_list <- reactiveValuesToList(input)
  to_toggle <- grepl("_x", names(input_list))
  input_list <- input_list[to_toggle]

  for (name in names(input_list)) {
    if (state == "OFF") {
      shinyjs::disable(name)
    } else {
      shinyjs::enable(name)
    }
  }
}

toggleTabs <- function(tabs = NULL, state = "OFF") {
  # tab_list <- paste0("[data-value='", tabs, "']")

  for (tab in tabs) {
    if (state == "OFF") {
      disable(selector = paste0("[data-value='", tab, "']"))
    } else {
      enable(selector = paste0("[data-value='", tab, "']"))
    }
  }
}


#--------------------------------------------------------------
# Save settings 
#--------------------------------------------------------------
shinyFileSave(input, "save_settings_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$save_settings_x, {
  if (length(input$save_settings_x) > 1) {
    session$doBookmark()
  }
})

onBookmarked(function(url) {
  state <- sub(".*(\\?_inputs_)", "", url)
  settings_path <- parseSavePath(volumes, input$save_settings_x)
  saveRDS(state, settings_path$datapath)
})


#--------------------------------------------------------------
# Load settings
#--------------------------------------------------------------
shinyFileChoose(input, "load_settings_x",
  roots = volumes, session = session,
  defaultRoot = default_root(), defaultPath = default_path()
)

observeEvent(input$load_settings_x, {
  settings_path <- parseFilePaths(volumes, input$load_settings_x)
  if (nrow(settings_path) > 0) {
    state <- tryCatch(readRDS(settings_path$datapath),
      error = function(e) NA
    )
    if (!is.na(state)) {
      url <- paste0(
        "http://", session$clientData$url_hostname, ":",
        session$clientData$url_port, "/?_inputs_", state
      )
      js$replace(url)
    }
  }
})


#--------------------------------------------------------------
# Reset trackR
#--------------------------------------------------------------
observeEvent(input$reset_x, {
  url <- paste0(
    "http://", session$clientData$url_hostname, ":",
    session$clientData$url_port
  )
  js$replace(url)
})


#--------------------------------------------------------------
# Bookmarking
#--------------------------------------------------------------
setBookmarkExclude(c(
  session$getBookmarkExclude(), "save_settings_x",
  "save_settings_x-modal", "load_settings_x", "reset_x"
))
