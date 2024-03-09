# Toggle UI on and off during long operations
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
  tab_list <- paste0("[data-value='", tabs, "']")

  for (tab in tabs) {
    if (state == "OFF") {
      disable(selector = paste0("[data-value='", tab, "']"))
    } else {
      enable(selector = paste0("[data-value='", tab, "']"))
    }
  }
}


# Save settings
shinyFileSave(input, "saveSettings_x", roots = volumes, session = session,
              defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$saveSettings_x, {
  if (length(input$saveSettings_x) > 1)
    session$doBookmark()
})

onBookmarked(function(url) {
  state <- sub(".*(\\?_inputs_)", "", url)
  settings_path <- parseSavePath(volumes, input$saveSettings_x)
  saveRDS(state, settings_path$datapath)
})

# Load settings
shinyFileChoose(input, "loadSettings_x", roots = volumes, session = session,
                defaultRoot = defaultRoot(), defaultPath = defaultPath())

observeEvent(input$loadSettings_x, {
  settings_path <- parseFilePaths(volumes, input$loadSettings_x)
  if (nrow(settings_path) > 0) {
    state <- tryCatch(readRDS(settings_path$datapath),
                        error = function(e) NA)
    if (!is.na(state)) {
      url <- paste0("http://", session$clientData$url_hostname, ":",
                    session$clientData$url_port, "/?_inputs_", state)
      js$replace(url)
    }
  }
})

# Reset trackR
observeEvent(input$reset_x, {
  url <- paste0("http://", session$clientData$url_hostname, ":",
                session$clientData$url_port)
  js$replace(url)
})

# Bookmark
setBookmarkExclude(c(session$getBookmarkExclude(), "saveSettings_x",
                     "saveSettings_x-modal", "loadSettings_x", "reset_x"))
