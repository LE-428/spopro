library(shiny)
library(bslib)
library(R.utils)
library(plotly)

options(shiny.maxRequestSize = 100*1024^2)  # Erhöht das Limit auf 50 MB


# Lade deine Funktionen
source("Run/run_all_nonrecursive.R")


ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),  # Ändere das Theme auf "darkly"
  # Zentrieren mit div + text-align: center
  tags$div(
    tags$h1("Spotify Data Analyzer", 
            style = "color: #1DB954; font-weight: bold; font-family: 'Consolas', sans-serif;"),
    tags$p("Just like Spotify Wrapped, but digging deeper", style = "font-size: 14px; color: gray;"),
    style = "text-align: center;"
  ),
  sidebarLayout(
    sidebarPanel(
      
      tags$div(
        tags$h4("Upload your JSON Files", style = "font-weight: bold; margin-top: 14px;"),
      ),
      
      fileInput("file", 
                tags$span("(request your Extended Streaming History from ", 
                          tags$a("Spotify", href = "https://www.spotify.com/account/privacy/", target = "_blank"), 
                          ")"), 
                accept = ".json", 
                multiple = TRUE)
      ,
      
      tags$div(
        tags$h4("Load additional stats", style = "font-weight: bold; margin-top: 14px;"),
      ),
      
      # Button zum manuellen Abrufen der API-Daten
      actionButton("get_api_data", "Fetch API Data"),
      
      
      textOutput("api_status"),  # Zeigt den API-Status an
      
      tags$div(
        tags$h4("Use your API credentials", tags$a("Documentation", 
          href = "https://developer.spotify.com/documentation/web-api", style = "color: gray; font-size: 14px;"),
          style = "font-weight: bold; margin-top: 14px;"),
      ),
      
      
      textInput("client_id", "Spotify Client ID", placeholder = "Enter your Client ID here"),
      textInput("client_secret", "Spotify Client Secret", placeholder = "Enter your Client Secret here"),
      actionButton("save_creds", "Save Credentials"),
      
      tags$div(
        tags$h4("Ideas? Suggestions?", style = "font-weight: bold; margin-top: 14px;"),
        tags$a("GitHub", href = "https://github.com/LE-428/spopro", style = "color: gray; font-size: 14px;")
      )
      
    ),
    mainPanel(
      
      verbatimTextOutput("demo_comment"),
      
      br(),
      
      verbatimTextOutput("quick_stats_comment"),
      # Textausgabe für quick_stats
      verbatimTextOutput("quick_stats_text"),
      
      br(),
      br(),
      
      verbatimTextOutput("country_plot_comment"),
      plotOutput("country_plot"),
      
      br(),
      
      tableOutput("country_table"),
      
      br(),
      br(),
      
      
      # Plot: Plattformen
      verbatimTextOutput("platform_usage_comment"),
      plotOutput("platform_usage_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("listening_time_comment"),
      # Textausgabe für die Listening Time pro Jahr
      tableOutput("listening_time"),
      
      br(),
      br(),
      
      verbatimTextOutput("time_year_plot_comment"),
      plotOutput("time_year_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("activity_time_plot_comment"),
      plotOutput("activity_time_plot"),
      
      br(),
      br(),
      
      # Tabelle: Top Days Time
      verbatimTextOutput("top_days_comment"),
      tableOutput("top_days_table"),
      
      br(),
      br(),
      
      # Tabelle: Top Albums
      verbatimTextOutput("top_albums_comment"),
      selectInput("selected_year_e", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      tableOutput("top_albums_table_recent"),
      
      br(),
      
      tableOutput("top_albums_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_album_comment"),
      # Album search bar and dropdown
      selectizeInput("search_album", "Choose or search", choices = NULL),
      # verbatimTextOutput("top_album_name_comment"),
      tableOutput("top_album_tracks_table"),
      
      br(),
      
      plotOutput("album_time_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("track_per_year_comment"),
      selectInput("selected_year_c", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      tableOutput("track_per_year_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_days_streak_comment"),
      selectInput("selected_year_f", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      tableOutput("artist_days_streak_table"),
      
      br(),
      
      tableOutput("artist_days_streak_table_all"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_artists_comment"),
      selectInput("selected_year_d", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      tableOutput("top_artists_table_recent"),
      
      br(),
      
      tableOutput("top_artists_table"),
      
      br(),
      
      verbatimTextOutput("artist_albums_comment"),
      # Search bar
      selectizeInput("search_artist_b", "Choose or search", choices = NULL),
      tableOutput("artist_albums_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_artist_comment"),
      # Search bar
      selectizeInput("search_artist", "Choose or search", choices = NULL),
      tableOutput("top_artist_tracks_table"),
      
      br(),
      
      plotOutput("artist_time_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_featured_artists_comment"),
      tableOutput("top_featured_artists_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_month_comment"),
      selectInput("selected_year", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      tableOutput("artist_month_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_ranking_comment"),
      selectInput("selected_year_g", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      plotOutput("artist_ranking_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("time_artist_activity_comment"),
      tableOutput("time_artist_activity_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_tracks_comment"),
      tableOutput("top_tracks_table_all"),
      
      br(),
      
      selectizeInput("search_track", "Choose or search", choices = NULL),
      plotOutput("track_time_plot"),
      
      br(),
      
      selectInput("selected_year_b", "Choose a year",
                  choices = NULL,  # Will be filled later
                  selected = NULL), # Default
      tableOutput("top_tracks_table_recent"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_tracks_incognito_comment"),
      tableOutput("top_tracks_incognito_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("longest_tracks_comment"),
      tableOutput("longest_tracks_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_tracks_over_time_plot_comment"),
      plotOutput("top_tracks_over_time_plot", width = "800px", height = "600px"),
      br(),
      downloadButton("download_pdf", "Download Plot PDF"),
      
      br(),
      br(),
      
      verbatimTextOutput("most_skipped_comment"),
      tableOutput("most_skipped_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("activity_month_plot_comment"),
      plotOutput("activity_month_plot_all"),
      
      br(),
      br(),
      
      verbatimTextOutput("recent_year_text"),
      plotOutput("activity_month_plot_recent"),
      
      
      ### EXTENDED
      
      br(),
      
      verbatimTextOutput("extended_comment"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_graph_comment"),
      plotlyOutput("artist_graph_plot", height = "600px"),
      br(),
      downloadButton("download_pdf_b", "Download Plot PDF"),
      
      br(),
      br(),
      
      verbatimTextOutput("track_duration_plot_comment"),
      plotOutput("track_duration_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("release_years_plot_comment"),
      plotOutput("release_years_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_follower_plot_comment"),
      plotOutput("artist_follower_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_popularity_plot_comment"),
      plotOutput("artist_popularity_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_genres_comment"),
      plotOutput("top_genres_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_genre_comment"),
      tableOutput("artist_genre_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("month_genre_activity_comment"),
      tableOutput("month_genre_activity_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("time_genre_activity_comment"),
      tableOutput("time_genre_activity_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("evaluate_music_taste_comment"),
      verbatimTextOutput("evaluate_music_taste_text"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_cluster_plot_comment"),
      plotOutput("artist_cluster_plot_plot", width = "800px", height = "600px"),
      
      br(),
      br(),
      
      verbatimTextOutput("distribution_shuffle_comment"),
      plotOutput("distribution_shuffle_plot"),
    )
  )
)





server <- function(input, output, session) {
  
  # Standard-Daten aus einer Demo-CSV im Projektverzeichnis laden
  default_data <- read.csv("demo.csv")  # Ersetze durch deinen tatsächlichen Dateipfad
  
  # Standardwerte für die Reactives setzen
  data_combined <- reactiveVal(default_data)
  data_extended <- reactiveVal(default_data)
  
  demo_comment <- reactiveVal("SHOWING DEMO DATASET")
  extended_comment <- reactiveVal("EXTENDED DATA (API) NEEDED FROM HERE ON")
  
  
  data_json <- reactive({
    req(input$file)
    temp_dir <- tempdir()
    file_paths <- input$file$datapath
    file_names <- input$file$name
    for (i in seq_along(file_paths)) {
      file.copy(file_paths[i], file.path(temp_dir, file_names[i]))
    }
    your_dataframe <- load_YOUR_data(temp_dir)  # Dein Leseprozess
    your_dataframe <- drop_podcasts(your_dataframe)
    return(your_dataframe)
  })
  
  
  # Beobachte Änderungen bei `data_json()` und aktualisiere `data_combined`
  observeEvent(data_json(), {
    data_combined(NULL)
    data_combined(data_json())  # Überschreibt den Wert mit den neuen JSON-Daten
    data_extended(NULL)  # Setzt data_extended zurück (leert es), bevor neue Daten geladen werden
    demo_comment("")
    extended_comment("")
  })
  
  
  observeEvent(input$save_creds, {
    if (nzchar(input$client_id) && nzchar(input$client_secret)) {
      set_api_creds(input$client_id, input$client_secret)  # API-Creds setzen
      source("spotify_api_access.r")
      showNotification("Spotify API credentials updated!", type = "message")
    } else {
      showNotification("Please enter both Client ID and Client Secret.", type = "error")
    }
  })
   
   
   # API-Daten laden (asynchron, ohne die anderen Render-Blöcke zu blockieren)
   # data_extended <- reactiveVal(NULL)  # Erstmal NULL, später aktualisiert
   
   api_status <- reactiveVal("Waiting for upload...")
   

   
   observeEvent(input$get_api_data, {
     if (is.null(data_json()) || nrow(data_json()) == 0) {
       api_status("Please upload a valid JSON file first.")  
     } else {
       api_status("API is processing, please wait...")  
       
       tryCatch({
         extended_dataframe <- withTimeout({
           add_api_data(data_frame = data_json(), access = access_token, write_to_csv = FALSE, playtime_threshold = 5000)
         }, timeout = 300, onTimeout = "error")  # Abbruch nach 10 Sekunden
         
         data_extended(extended_dataframe)  
         api_status("Successfully loaded API data")  
       }, error = function(e) {
         api_status(paste("Error occurred:", e$message, "Please use your own credentials or try again later."))
       })
     }
   })
   
   # List of Top 10 albums for dropdown menu
   top_albums_list <- reactive({
     req(data_combined())  
     top_albums(data_combined(), top_x = 10)$Album  # Gibt die Album-Namen zurück
   })
   
   # Refresh dropdown
   observe({
     updateSelectInput(session, "selected_album",
                       choices = top_albums_list(),
                       selected = top_albums_list()[1])  # Erstes Album als Standard
   })
   
   
   
   # List of Top 10 artists for dropdown menu
   top_artists_list <- reactive({
     req(data_combined())  
     top_artists(data_combined(), top_x = 15)$Artist  # Gibt die Artist-Namen zurück
   })
   
   
   # List of available years
   years_list <- reactive({
     req(data_combined())  
     sort(unique(substr(data_combined()$ts, 1, 4)))
   })
   
   # Refresh dropdown
   observe({
     updateSelectInput(session, "selected_year",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   
   # List of available tracks
   top_tracks_list <- reactive({
     req(data_combined())  
     top_tracks(data_combined(), top_x = 10)$Track
   })
   
   # List of available artists
   all_artists_list <- reactive({
     req(data_combined())  
     unique(c(top_artists_list(), "---------------", data_combined()$master_metadata_album_artist_name))
   })
   
   # List of available albums
   all_albums_list <- reactive({
     req(data_combined())  
     unique(c(top_albums_list(), "---------------", data_combined()$master_metadata_album_album_name))
   })
   
   # List of available albums
   all_tracks_list <- reactive({
     req(data_combined())  
     unique(c(top_tracks_list(), "---------------", data_combined()$master_metadata_track_name))
   })
   
   # Update the dropdown menu after dataframe has been read (artists)
   observeEvent(data_combined(), {
     updateSelectizeInput(session, "search_artist", choices = all_artists_list(), server = TRUE)
   })
   
   # Update the dropdown menu after dataframe has been read (most popular albums of selected artist)
   observeEvent(data_combined(), {
     updateSelectizeInput(session, "search_artist_b", choices = all_artists_list(), server = TRUE)
   })
   
   # Update the dropdown menu after dataframe has been read (albums)
   observeEvent(data_combined(), {
     updateSelectizeInput(session, "search_album", choices = all_albums_list(), server = TRUE)
   })
   
   # Update the dropdown menu after dataframe has been read (tracks)
   observeEvent(data_combined(), {
     updateSelectizeInput(session, "search_track", choices = all_tracks_list(), server = TRUE)
   })
   
   
   # Refresh second year dropdown menu
   observe({
     updateSelectInput(session, "selected_year_b",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   # Refresh third year dropdown menu
   observe({
     updateSelectInput(session, "selected_year_c",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   # Refresh fourth year dropdown menu
   observe({
     updateSelectInput(session, "selected_year_d",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   # Refresh fifth year dropdown menu
   observe({
     updateSelectInput(session, "selected_year_e",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   # Refresh sixth year dropdown menu
   observe({
     updateSelectInput(session, "selected_year_f",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   # Refresh seventh year dropdown menu
   observe({
     updateSelectInput(session, "selected_year_g",
                       choices = years_list(),
                       selected = years_list()[pmax(1, length(years_list()) - 1)])  # Last completed year as default
   })
   
   
   output$demo_comment <- renderText({
     demo_comment()
   })
   
   output$api_status <- renderText({
     api_status()
   })
   
  
  # Kommentarblock
  output$quick_stats_comment <- renderPrint({
    req(data_combined())
    cat("Quick overview of the dataset\n")
  })
  
  # Textblock: quick_stats() Ausgabe
  output$quick_stats_text <- renderText({
    req(data_combined())
    quick_stats(data_combined())
  })
  
  # Textblock: Kommentar zu country_plot und Ausgabe des Plots
  output$country_plot_comment <- renderPrint({
    req(data_combined())
    cat("Countries you have listened music in (connecting country)\n")
  })
  
  output$country_plot <- renderPlot({
    req(data_combined())
    country_plot(data_combined())
  })
  
  output$country_table <- renderTable({
    req(data_combined())
    country_list(data_combined())
  })
  
  # Plot: platform_usage()
  output$platform_usage_comment <- renderPrint({
    req(data_combined())
    cat("Spotify usage on different platforms")
  })
  
  output$platform_usage_plot <- renderPlot({
    req(data_combined())
    platform_usage_plot(data_combined())
  })
  
  # Kommentarblock
  output$listening_time_comment <- renderPrint({
    req(data_combined())
    cat("Total listening time of all recorded streams in minutes")
  })
  
  # Textblock: Listening Time pro Jahr
  output$listening_time <- renderTable({
    req(data_combined())
    time_year(data_combined())
  })
  
  # Textblock: Kommentar zu time_year_plot und Ausgabe des Plots
  output$time_year_plot_comment <- renderPrint({
    req(data_combined())
    cat("Listening time over the years\n")
  })
  
  output$time_year_plot <- renderPlot({
    req(data_combined())
    time_year_plot(data_combined())
  })
  
  # Textblock: Kommentar zu activity_time_plot und Ausgabe des Plots
  output$activity_time_plot_comment <- renderPrint({
    req(data_combined())
    cat("Minutes played per hour\n")
  })
  
  output$activity_time_plot <- renderPlot({
    req(data_combined())
    activity_time_plot(data_combined())
  })
  
  #  Kommentarblock
  output$top_days_comment <- renderPrint({
    req(data_combined())
    cat("Days with most listening activity")
  })
  
  # Tabelle: Top Days Time
  output$top_days_table <- renderTable({
    req(data_combined())
    top_days_time(data_combined(), top_x = 10)
  })
  
  # Kommentarblock
  output$top_albums_comment <- renderPrint({
    req(data_combined())
    cat("Most listened albums (selected year and all time)")
  })
  
  # Tabelle: Top Albums nach Jahr
  output$top_albums_table_recent <- renderTable({
    req(data_combined())
    recent_year <- input$selected_year_e
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    top_albums(recent_year_dataframe, top_x = 10)
  })
  
  # Tabelle: Top Albums
  output$top_albums_table <- renderTable({
    req(data_combined())
    top_albums(data_combined(), top_x = 10)
  })
  
  # Top Album
  output$top_album_comment <- renderPrint({
    req(data_combined())
    cat("Track listen counter for your favorite album\n")
  })
  
  
  output$top_album_tracks_table <- renderTable({
    req(data_combined())
    selected_album_escaped <- as.character(input$search_album)
    top_album_tracks(data_combined(), album_string = selected_album_escaped, exact_search_bool = TRUE)
  })
  
  
  output$album_time_plot <- renderPlot({
    req(data_combined())
    selected_album_escaped <- as.character(input$search_album)
    if (selected_album_escaped != "---------------"){
      album_time_plot(data_combined(), album_string = selected_album_escaped, exact = TRUE)
    }
  })
  
  # Textblock: Kommentar zu track_per_year und Ausgabe der Tabelle
  output$track_per_year_comment <- renderPrint({
    req(data_combined())
    cat("Tracks that you have listened to the most often within one day\n")
  })
  
  output$track_per_year_table <- renderTable({
    req(data_combined(), input$selected_year_c)
    recent_year <- input$selected_year_c
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    track_per_year(recent_year_dataframe, top_x = 10)
  })
  
  # Listening streaks
  output$artist_days_streak_comment <- renderPrint({
    req(data_combined())
    cat("Longest streaks of continuous playback (of certain artist), selected year and all time")
  })
  
  output$artist_days_streak_table <- renderTable({
    req(data_combined(), input$selected_year_f)
    recent_year <- input$selected_year_f
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    artist_days_streak(recent_year_dataframe, top_x = 6)
  })
  
  output$artist_days_streak_table_all <- renderTable({
    req(data_combined())
    artist_days_streak(data_combined(), top_x = 4)
  })
  
  # Textblock: Kommentar zu top_artists und Ausgabe der Tabelle
  output$top_artists_comment <- renderPrint({
    req(data_combined())
    cat("Top artists by minutes played (selected year and all time)\n")
  })
  
  output$top_artists_table_recent <- renderTable({
    req(data_combined(), input$selected_year_d)
    recent_year <- input$selected_year_d
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    top_artists(recent_year_dataframe, top_x = 10)
  })
  
  output$top_artists_table <- renderTable({
    req(data_combined())
    top_artists(data_combined(), top_x = 15)
  })
  
  # Textblock: Kommentar zu artist_albums und Ausgabe der Tabelle
  output$artist_albums_comment <- renderPrint({
    req(data_combined())
    cat("Most popular albums of selected artist")
  })
  
  
  output$artist_albums_table <- renderTable({
    req(data_combined())
    selected_artist_escaped <- as.character(input$search_artist_b)
    artist_albums(data_combined(), artist_string = selected_artist_escaped, exact = TRUE, top_x = 5)
  })
  
  # Textblock: Kommentar zu top_artist_name und Ausgabe der Tabelle
  output$top_artist_comment <- renderPrint({
    req(data_combined())
    cat("Top tracks of selected artist\n")
  })
  
  
  output$top_artist_tracks_table <- renderTable({
    req(data_combined())
    # selected_artist_escaped <- as.character(input$selected_artist)
    selected_artist_escaped <- as.character(input$search_artist)
    artist_top_tracks(data_combined(), top_x = 10, artist_string = selected_artist_escaped, exact = TRUE)
  })
  
  output$artist_time_plot <- renderPlot({
    req(data_combined())
    # selected_artist_escaped <- as.character(input$selected_artist)
    selected_artist_escaped <- as.character(input$search_artist)
    if (selected_artist_escaped != "---------------"){
      artist_time_plot(data_combined(), artist_string = selected_artist_escaped)
    }
  })
  
  # Textblock: Kommentar zu top_featured_artists und Ausgabe der Tabelle
  output$top_featured_artists_comment <- renderPrint({
    req(data_combined())
    cat("Artists that appear most often in songs with features\n")
  })
  
  output$top_featured_artists_table <- renderTable({
    req(data_combined())
    top_featured_artists(data_combined(), top_x = 10)
  })
  
  # Textblock: Kommentar zu artist_month und Ausgabe der Tabelle
  output$artist_month_comment <- renderPrint({
    req(data_combined())
    cat("Top artists of each month by minutes played (recent year)\n")
  })
  
  
  output$artist_month_table <- renderTable({
    req(data_combined())
    # years <- sort(unique(substr(data_combined()$ts, 1, 4)))
    # recent_year <- years[pmax(1, length(years) - 1)]
    recent_year <- input$selected_year
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    artist_month(recent_year_dataframe)
  })
  
  # Textblock: Kommentar zu top_artist_ranking_plot
  output$artist_ranking_comment <- renderPrint({
    req(data_combined())
    cat("Top 10 artists with cumulated listening time")
  })
  
  
  output$artist_ranking_table <- renderPlot({
    req(data_combined())
    # years <- sort(unique(substr(data_combined()$ts, 1, 4)))
    # recent_year <- years[pmax(1, length(years) - 1)]
    recent_year <- input$selected_year_g
    top_artists_ranking_plot(data_combined(), recent_year)
  })
  
  # Textblock: Kommentar zu time_artist_activity und Ausgabe der Tabelle
  output$time_artist_activity_comment <- renderPrint({
    req(data_combined())
    cat("Top artists by time of day\n")
  })
  
  output$time_artist_activity_table <- renderTable({
    req(data_combined())
    time_artist_activity(data_combined())
  })
  
  # Textblock: Kommentar zu top_tracks (All time und Recent Year) und Ausgabe der Tabellen
  output$top_tracks_comment <- renderPrint({
    req(data_combined())
    cat("Tracks that have been played most often (all time and last completed year)\n")
  })
  
  output$top_tracks_table_all <- renderTable({
    req(data_combined())
    top_tracks(data_combined(), top_x = 10)
  })
  
  output$track_time_plot <- renderPlot({
    req(data_combined())
    selected_track_escaped <- as.character(input$search_track)
    if (selected_track_escaped != "---------------"){
      track_time_plot(data_combined(), track_string = selected_track_escaped, exact = TRUE)
    }
  })
  
  output$top_tracks_table_recent <- renderTable({
    req(data_combined())
    # years <- sort(unique(substr(data_combined()$ts, 1, 4)))
    # recent_year <- years[pmax(1, length(years) - 1)]
    recent_year_b <- input$selected_year_b
    recent_year_dataframe <- extract_year(recent_year_b, data_combined())
    top_tracks(recent_year_dataframe, top_x = 10)
  })
  
  # Textblock: Kommentar zu top_tracks (Incognito Mode) und Ausgabe der Tabelle
  output$top_tracks_incognito_comment <- renderPrint({
    req(data_combined())
    cat("Most popular tracks with incognito mode enabled\n")
  })
  
  output$top_tracks_incognito_table <- renderTable({
    req(data_combined())
    top_tracks(incognito(data_combined()), top_x = 10)
  })
  
  # Textblock: Kommentar zu top_tracks_over_time_plot und Ausgabe des Plots
  output$top_tracks_over_time_plot_comment <- renderPrint({
    req(data_combined())
    cat("Plot/pdf with your Top 100 songs over the years\n")
  })
  
  output$top_tracks_over_time_plot <- renderPlot({
    req(data_combined())
    top_tracks_over_time_plot(data_combined(), save_plot = TRUE)
  })
  
  # Serverlogik zum Erstellen und Bereitstellen des PDFs
  output$download_pdf <- downloadHandler(
    filename = paste("top_tracks_plot.pdf"),
    content = function(file) {
      # Erstelle das PDF mit ggsave() im aktuellen Arbeitsverzeichnis
      file.copy("top_tracks_plot.pdf", file)
    }
  )

  
  # Textblock: Kommentar zu most_skipped und Ausgabe der Tabelle
  output$most_skipped_comment <- renderPrint({
    req(data_combined())
    cat("Tracks that have been skipped most often\n")
  })
  
  output$most_skipped_table <- renderTable({
    req(data_combined())
    most_skipped(data_combined(), top_x = 10)
  })
  
  # Textblock: Kommentar zu activity_month_plot und Ausgabe der Plots
  output$activity_month_plot_comment <- renderPrint({
    req(data_combined())
    cat("Minutes played per month (all time and recent year)\n")
  })
  
  output$activity_month_plot_all <- renderPlot({
    req(data_combined())
    activity_month_plot(data_combined())
  })
  
  output$recent_year_text <- renderPrint({
    req(data_combined())
    cat("Recent year:\n")
  })
  
  output$activity_month_plot_recent <- renderPlot({
    req(data_combined())
    years <- sort(unique(substr(data_combined()$ts, 1, 4)))
    recent_year <- years[pmax(1, length(years) - 1)]
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    activity_month_plot(recent_year_dataframe)
  })
  
  
  ### EXTENDED DATAFRAME
  
  # Textblock: Kommentar zu artist_graph_plot
  output$artist_graph_comment <- renderPrint({
    req(data_extended())
    cat("Graph with features represented as edges between artists")
  })
  
  output$artist_graph_plot <- renderPlotly({
    req(data_extended())
    p <- artist_graph_plot(data_extended(), extrapolate_genres = TRUE, save_plot = TRUE)
    ggplotly(p, tooltip = "text")
  })
  
  # Serverlogik zum Erstellen und Bereitstellen des PDFs
  output$download_pdf_b <- downloadHandler(
    filename = paste("artist_graph.pdf"),
    content = function(file) {
      # Erstelle das PDF mit ggsave() im aktuellen Arbeitsverzeichnis
      file.copy("artist_graph.pdf", file)
    }
  )
  
  # Textblock: Kommentar zu track_duration_plot und Ausgabe des Plots
  output$track_duration_plot_comment <- renderPrint({
    req(data_extended())
    cat("Distribution of song lengths of all streams\n")
  })
  
  output$track_duration_plot <- renderPlot({
    req(data_extended())
    track_duration_plot(data_extended(), TRUE)
  })
  
  # Tabelle: längste Lieder
  output$longest_tracks_comment <- renderPrint({
    req(data_extended())
    cat("Longest tracks that were listened")
  })
  
  output$longest_tracks_table <- renderTable({
    req(data_extended())
    longest_tracks(data_extended())
  })
  
  # Textblock: Kommentar zu release_years_plot und Ausgabe des Plots
  output$release_years_plot_comment <- renderPrint({
    req(data_extended())
    cat("Release years of the songs streamed\n")
  })
  
  output$release_years_plot <- renderPlot({
    req(data_extended())
    release_years_plot(data_extended())
  })
  
  # Kommentar und Plot: Distribution of streams and follower number
  output$artist_follower_plot_comment <- renderPrint({
    req(data_extended())
    cat("Distribution of the streams and the follower number of the artist\n")
  })
  
  output$artist_follower_plot <- renderPlot({
    req(data_extended())
    artist_follower_plot(data_extended(), TRUE)
  })
  
  # Kommentar und Plot: Popularity of artist over amount of streams
  output$artist_popularity_plot_comment <- renderPrint({
    req(data_extended())
    cat("Popularity of artist over amount of streams\n")
  })
  
  output$artist_popularity_plot <- renderPlot({
    req(data_extended())
    artist_popularity_plot(data_extended(), TRUE)
  })
  
  # Kommentar und Tabelle: Top Genres
  output$top_genres_comment <- renderPrint({
    req(data_extended())
    cat("Top genres\n")
  })
  
  output$top_genres_plot <- renderPlot({
    req(data_extended())
    top_genres_plot(data_extended(), top_x = 10)
  })
  
  # Kommentar und Tabelle: Top Artists je Genre
  output$artist_genre_comment <- renderPrint({
    req(data_extended())
    cat("Favorite artist per genre\n")
  })
  
  #  Tabelle: Top Artist je Genre
  output$artist_genre_table <- renderTable({
    req(data_extended())
    top_artist_by_genre(data_extended(), top_x = 10)
  })
  
  # Kommentar und Tabelle: Top Genre by Month
  output$month_genre_activity_comment <- renderPrint({
    req(data_extended())
    cat("Top genre by month (most recent completed year)\n")
  })
  
  output$month_genre_activity_table <- renderTable({
    req(data_extended())
    years <- sort(unique(substr(data_combined()$ts, 1, 4)))
    recent_year <- years[pmax(1, length(years) - 1)]
    recent_year_dataframe <- extract_year(recent_year, data_extended())
    month_genre_activity(recent_year_dataframe)
  })
  
  # Kommentar und Tabelle: Top Genre by Daytime
  output$time_genre_activity_comment <- renderPrint({
    req(data_extended())
    cat("Top genre by daytime\n")
  })
  
  output$time_genre_activity_table <- renderTable({
    req(data_extended())
    out <- time_genre_activity(data_extended())
    out <- out %>% rename(
      Hour = hour, Top_Genre = top_genre, Frequency = top_genre_freq
    )
    out
  })
  
  # Kommentar und Textblock: Evaluate Music Taste
  output$evaluate_music_taste_comment <- renderPrint({
    req(data_extended())
    cat("Evaluate the exquisiteness of your music taste\n")
  })
  
  output$evaluate_music_taste_text <- renderText({
    req(data_extended())
    evaluate_music_taste(data_extended())
  })
  
  # Kommentar und Textblock: Artist Cluster Plot
  output$artist_cluster_plot_comment <- renderPrint({
    req(data_extended())
    cat("Scatterplot with one point per artist: Artist with follower count and average song length")
  })
  
  output$artist_cluster_plot_plot <- renderPlot({
    req(data_extended())
    artist_cluster_plot(data_extended())
  })
  
  # Kommentar und Plot: Shuffle/Skip-Plot
  output$distribution_shuffle_comment <- renderPrint({
    req(data_extended())
    cat("Plot showcasing the different phases of track listening: early skipping,
        stable behavior during the middle, early track ending (outro etc.),
        more early and less late skips with shuffled tracks, 
        user not actively controlling playback?")
  })
  
  output$distribution_shuffle_plot <- renderPlot({
    req(data_extended())
    duration_distribution_plot(data_extended())
  })
  
}

shinyApp(ui, server)
