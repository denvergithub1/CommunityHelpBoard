#install.packages("shiny")
#install.packages("googlesheets4")
#install.packages("dplyr")
#install.packages("DT")

library(shiny)
library(googlesheets4)
library(dplyr)
library(DT)

# Authenticating for back end link. This can change per country. 
gs4_auth(path = "....")  

# Private back-end sheet (use Sheet URL)
Default_Sheet_ID <- "...."

# UI set up as prescribed by Shiny Site. 
ui <- fluidPage(
  titlePanel("Lucia Neighborhood Resource Board"),
  
  
  # Show posts first
  fluidRow(
    column(
      width = 12,
      DTOutput("postTable"),
      tableOutput("categoryRatings"),
      hr()
    )
  ),
  
  selectInput("filterCategory", "Search for Resource, by Category:",
              choices = c("Highlights","All", "Jobs", "Food", "Education", "Transport", "Emergency", "Holiday", "Advertisement","Sports", "Other")),
  
  hr(),
      textInput("title", "Post your need here (e.g., Need a Job):"),
  
      selectInput("category", "Category:",
                  choices = c("Jobs", "Food", "Education", "Transport", "Emergency", "Alert to correct mistake/ report abuse", "Other")),
      textAreaInput("description", "Description/Contact:"),
      
      checkboxInput("showMediaFields", "Add Image/Audio?", FALSE),
      
      conditionalPanel(
        condition = "input.showMediaFields == true",
        textInput("imageURL", "Image URL (optional):"),
        textInput("audioURL", "Audio URL (optional):")
      ),
      
      actionButton("submit", "Submit Post"),
  actionButton("refresh", "Refresh Posts"),
      hr(),
  
  
  p(strong("Headers for Uploaded Sheet:"),
    "Title | Category | Description | ImageURL | AudioURL | Rating | Timestamp",
    style = "color: black; margin-bottom: 20px;"),
  

  sidebarLayout(
    sidebarPanel(
      tagList(
        actionButton("toggleSheetInput", "Click to upload own Google Sheet"),
        actionButton("resetSheet", "Reset to Resource Board", icon = icon("undo")),
        
        uiOutput("sheetInputUI"),
        
        actionButton("loadSheet", "Load Sheet"),
        
        
        helpText("If uploading your own sheet, 'Anyone with the link can edit if shared'.")
      ),
      
     
    ),
      
      
    mainPanel(
      tags$div(
        tags$style(HTML(".green-label { color: green; }")),
        HTML("<p class='green-label'>JSur copyright ©2023</p> Sponsors:"),
        p("Silver Sponsor: JnBaptiste Foundation", br(), "Gold sponsor: AGM Network Solution Advance Caribbean Businesses")
      )
    )
  )
)

server <- function(input, output, session) {
  sheet_id <- reactiveVal(Default_Sheet_ID)  # This sets default sheet on start
  posts <- reactiveVal(data.frame())
  
  sheetInputVisible <- reactiveVal(FALSE)
  
  observeEvent(input$resetSheet, {
    sheet_id(Default_Sheet_ID)
    
    tryCatch({
      df <- read_sheet(sheet_id())
      posts(df)
      showNotification("Reset to default sheet.", type = "message")
    }, error = function(e) {
      showNotification("Failed to reset to default sheet.", type = "error")
    })
    
    sheetInputVisible(FALSE)
  })
  
  observeEvent(input$toggleSheetInput, {
    sheetInputVisible(!sheetInputVisible())  # Toggle show/hide
  })
  
  output$sheetInputUI <- renderUI({
    if (sheetInputVisible()) {
      tagList(
        textInput("sheetURL", "Enter Google Sheet URL:")
      )
    }
  })
  
  # load the default sheet when launched
  observe({
    isolate({
      tryCatch({
        df <- read_sheet(sheet_id())
        posts(df)
      }, error = function(e) {
        showNotification("Failed to load default sheet on startup.", type = "error")
      })
    })
  })
  
  #  If there is no sheet uploaded, an error will not show because of this addition.
  observeEvent(input$loadSheet, {
    if (!is.null(input$sheetURL) && nzchar(input$sheetURL)) {
      id <- sub(".*?/d/([a-zA-Z0-9_-]+).*", "\\1", input$sheetURL)
      sheet_id(id)
    } else {
      sheet_id(Default_Sheet_ID)
    }
    
    tryCatch({
      df <- read_sheet(sheet_id())
      
      required_cols <- c("Title", "Category", "Description", "ImageURL", "AudioURL", "Rating", "Timestamp")
      if (nrow(df) == 0 || !all(required_cols %in% names(df))) {
        empty_df <- data.frame(
          Title = character(),
          Category = character(),
          Description = character(),
          ImageURL = character(),
          AudioURL = character(),
          Rating = numeric(),
          Timestamp = character()
        )
        range_write(sheet_id(), data = empty_df, sheet = 1, col_names = TRUE)
        posts(empty_df)
      } else {
        posts(df)
      }
      
      showNotification("Sheet loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification("Failed to load sheet. Ensure it exists and is shared.", type = "error")
    })
  })
  
  loadPosts <- function() {
    tryCatch({
      req(sheet_id())
      posts(NULL)  # Clear existing posts
      df <- read_sheet(sheet_id())
      posts(df)
    }, error = function(e) {
      showNotification("Failed to load posts from sheet.", type = "error")
    })
  }
  
  observeEvent(input$submit, {
    req(sheet_id(), input$title, input$category, input$description)
    
    new_post <- data.frame(
      Title = input$title,
      Category = input$category,
      Description = input$description,
      ImageURL = if (!is.null(input$imageURL) && length(input$imageURL) == 1) input$imageURL else "",
      AudioURL = if (!is.null(input$audioURL) && length(input$audioURL) == 1) input$audioURL else "",
      Rating = NA_real_,
      Timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      sheet_append(sheet_id(), new_post)
      loadPosts()
      showNotification("Post submitted!", type = "message")
    }, error = function(e) {
      showNotification("Failed to submit post.", type = "error")
    })
  })
  
  observeEvent(input$refresh, {
    loadPosts()
  })
  
  output$postTable <- renderDT({
    df <- posts()
    req(nrow(df) > 0)
    
    if (input$filterCategory != "All" && "Category" %in% names(df)) {
      df <- df %>% filter(Category == input$filterCategory)
    }
    
    expected_cols <- c("Title", "Category", "Description", "ImageURL", "AudioURL", "Rating", "Timestamp")
    for (col in expected_cols) {
      if (!col %in% names(df)) df[[col]] <- NA
    }
    
    df$Image <- ifelse(!is.na(df$ImageURL) & df$ImageURL != "",
                       paste0("<img src='", df$ImageURL, "' height='60'>"),
                       "")
    df$Audio <- ifelse(!is.na(df$AudioURL) & df$AudioURL != "",
                       paste0("<audio controls src='", df$AudioURL, "'></audio>"),
                       "")
    
    datatable(
      df[, c("Title", "Category", "Description", "Rating", "Image", "Audio", "Timestamp")],
      escape = FALSE,
      options = list(pageLength = 10)
    )
  })
}

shinyApp(ui, server)
