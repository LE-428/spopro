library(shiny)
library(bslib)

options(shiny.maxRequestSize = 100*1024^2)  # Erhöht das Limit auf 50 MB


# Lade deine Funktionen
source("Run/run_all.R")


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
      fileInput("file", 
                tags$span("Upload JSON Files (request your Extended Streaming History from ", 
                          tags$a("Spotify", href = "https://www.spotify.com/account/privacy/", target = "_blank"), 
                          ")"), 
                accept = ".json", 
                multiple = TRUE)
      ,
      
      
      # Button zum manuellen Abrufen der API-Daten
      actionButton("get_api_data", "Fetch API Data"),
      
      
      textOutput("api_status"),  # Zeigt den API-Status an
      
      tags$div(
        tags$h4("Ideas? Suggestions?", style = "font-weight: bold; margin-top: 14px;"),
        tags$a("GitHub", href = "https://github.com/LE-428/spopro", style = "color: gray; font-size: 14px;")
      )
      
      
      # # Neue Textfelder für die Spotify Client ID und Client Secret
      # textInput("client_id", "Spotify Client ID", placeholder = "Enter your Client ID here"),
      # textInput("client_secret", "Spotify Client Secret", placeholder = "Enter your Client Secret here")
      
      # fileInput("file_csv", 
      #           tags$span("Upload CSV File (additional API-Data, view on ", 
      #                     tags$a("GitHub", href = "https://github.com/LE-428/spopro", target = "_blank"), 
      #                     ")"), 
      #           accept = ".csv", 
      #           multiple = FALSE)
    ),
    mainPanel(
      
      verbatimTextOutput("demo_comment"),
      
      br(),
      
      verbatimTextOutput("quick_stats_comment"),
      # Textausgabe für quick_stats
      verbatimTextOutput("quick_stats_text"),
      tableOutput("quick_stats_table"),
      
      br(),
      br(),
      
      # Plot: Plattformen
      verbatimTextOutput("platform_usage_comment"),
      plotOutput("platform_usage_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("listening_time_comment"),
      # Textausgabe für die Listening Time pro Jahr
      verbatimTextOutput("listening_time_text"),
      
      br(),
      br(),
      
      # Tabelle: Top Days Time
      verbatimTextOutput("top_days_comment"),
      tableOutput("top_days_table"),
      
      br(),
      br(),
      
      # Tabelle: Top Albums
      verbatimTextOutput("top_albums_comment"),
      tableOutput("top_albums_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_album_comment"),
      verbatimTextOutput("top_album_name_comment"),
      tableOutput("top_album_tracks_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("track_per_year_comment"),
      tableOutput("track_per_year_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_artists_comment"),
      tableOutput("top_artists_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_artist_comment"),
      verbatimTextOutput("top_artist_name_comment"),
      tableOutput("top_artist_tracks_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_featured_artists_comment"),
      tableOutput("top_featured_artists_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("artist_month_comment"),
      verbatimTextOutput("recent_year_comment"),
      tableOutput("artist_month_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("time_artist_activity_comment"),
      tableOutput("time_artist_activity_table"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_tracks_comment"),
      tableOutput("top_tracks_table_all"),
      
      br(),
      
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
      
      verbatimTextOutput("time_year_plot_comment"),
      plotOutput("time_year_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("activity_time_plot_comment"),
      plotOutput("activity_time_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("country_plot_comment"),
      plotOutput("country_plot"),
      
      br(),
      br(),
      
      verbatimTextOutput("top_tracks_over_time_plot_comment"),
      plotOutput("top_tracks_over_time_plot", width = "800px", height = "600px"),
      
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
      tableOutput("top_genres_table"),
      
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
    )
  )
)





server <- function(input, output) {
  
  # Standard-Daten aus einer Demo-CSV im Projektverzeichnis laden
  default_data <- read.csv("demo.csv")  # Ersetze durch deinen tatsächlichen Dateipfad
  
  # Standardwerte für die Reactives setzen
  data_combined <- reactiveVal(default_data)
  data_extended <- reactiveVal(default_data)
  
  demo_comment <- reactiveVal("SHOWING DEMO DATASET")
  
  
  data_json <- reactive({
    req(input$file)
    temp_dir <- tempdir()
    file_paths <- input$file$datapath
    file_names <- input$file$name
    for (i in seq_along(file_paths)) {
      file.copy(file_paths[i], file.path(temp_dir, file_names[i]))
    }
    your_dataframe <- load_YOUR_data(temp_dir)  # Dein Leseprozess
    return(your_dataframe)
  })
  
  # data_csv <- reactive({
  #   req(input$file_csv)
  #   your_extended_dataframe <- read.csv(input$file_csv$datapath)
  #   return(your_extended_dataframe)
  # })
  
  # Neue kombinierte Datenquelle:
  # data_combined <- reactive({
  #   if (!is.null(input$file)) {
  #     return(data_json())  # JSON wird priorisiert
  #   # } else if (!is.null(input$file_csv)) {
  #   #   return(data_csv())   # Falls keine JSON, nutze CSV
  #   } else {
  #     # return(default_data())  # Falls gar nichts hochgeladen wurde
  #   }
  # })
  
  # Beobachte Änderungen bei `data_json()` und aktualisiere `data_combined`
  observeEvent(data_json(), {
    data_combined(NULL)
    data_combined(data_json())  # Überschreibt den Wert mit den neuen JSON-Daten
    data_extended(NULL)  # Setzt data_extended zurück (leert es), bevor neue Daten geladen werden
    demo_comment("")
  })
  
  # # Versuche, `add_api_data` mit den JSON-Daten aufzurufen
  #  data_extended <- reactive({
  #   req(data_json())  # Warten auf die JSON-Daten
  #   tryCatch({
  #     # Versuche, die `add_api_data`-Funktion mit den JSON-Daten auszuführen
  #     # refresh_token()
  #     extended_dataframe <- add_api_data(data_table = data_json(), access = access_token, write_to_csv = FALSE)
  #     return(extended_dataframe)  # Erfolgreich, gebe die erweiterte Tabelle zurück
  #   }, error = function(e) {
  #     # Falls ein Fehler auftritt (z. B. keine JSON-Daten vorhanden sind), benutze die CSV-Daten
  #     message("Fehler bei der Verwendung der JSON-Daten, wechsle zu CSV-Daten.")
  #     # return(data_csv())  # Gib die CSV-Daten zurück
  #   })
  # })
   
   
   # API-Daten laden (asynchron, ohne die anderen Render-Blöcke zu blockieren)
   # data_extended <- reactiveVal(NULL)  # Erstmal NULL, später aktualisiert
   
   api_status <- reactiveVal("Waiting for upload...")
   

   # observeEvent(data_json(), {
   #   api_status("API is processing, please wait...")  # Status setzen
   #   
   #   tryCatch({
   #     # refresh_token()
   #     extended_dataframe <- add_api_data(data_table = data_json(), access = access_token, write_to_csv = FALSE)
   #     data_extended(extended_dataframe)  # Speichert die erweiterte Tabelle
   #     api_status("Successfully loaded API data")  # Erfolgsmeldung
   #   }, error = function(e) {
   #     message("Fehler bei API-Call: ", e$message)
   #     api_status(paste("Error ocurred when loading API data"))  # Fehlertext anzeigen
   #   })
   # })
   
   # Manuelle API-Datenabruf-Logik
   observeEvent(input$get_api_data, {
     # Überprüfen, ob data_json() verfügbar ist
     if (is.null(data_json()) || nrow(data_json()) == 0) {
       api_status("Please upload a valid JSON file first.")  # Hinweis, wenn keine JSON-Daten vorhanden sind
     } else {
       api_status("API is processing, please wait...")  # Status setzen
       
       tryCatch({
         # refresh_token()  # Hier ggf. Token-Refresh einfügen
         extended_dataframe <- add_api_data(data_table = data_json(), access = access_token, write_to_csv = FALSE)
         data_extended(extended_dataframe)  # Speichert die erweiterte Tabelle
         api_status("Successfully loaded API data")  # Erfolgsmeldung
       }, error = function(e) {
         message("Fehler bei API-Call: ", e$message)
         api_status(paste("Error occurred when loading API data:"))  # Fehlertext anzeigen
       })
     }
   })
   
   # # Versuche, `add_api_data` mit den JSON-Daten aufzurufen
   # data_extended <- reactive({
   #   req(data_csv())  # Warten auf die JSON-Daten
   #   if (!is.null(input$file_csv)) {
   #     return(data_csv())  # JSON wird priorisiert
   #   }
   # })
   
   output$demo_comment <- renderText({
     demo_comment()
   })
   
   output$api_status <- renderText({
     api_status()
   })
   
   # # Statusmeldung für die API
   # output$api_status <- renderText({
   #   if (is.null(data_extended())) {
   #     return("API is processing, please wait...")  # Wird angezeigt, wenn API noch läuft
   #   } else {
   #     return("")  # Kein Text, wenn API fertig ist
   #   }
   # })
  
  # Kommentarblock
  output$quick_stats_comment <- renderPrint({
    req(data_combined())
    cat("Get a quick overview of the dataset\n")
  })
  
  # Textblock: quick_stats() Ausgabe
  output$quick_stats_text <- renderPrint({
    req(data_combined())
    # capture.output fängt alle print-Ausgaben ab und gibt sie als Vektor zurück
    txt <- capture.output({
      quick_stats(data_combined())
    })
    cat(txt, sep = "\n")
  })
  
  # Tabelle: quick_stats() Ausgabe
  output$quick_stats_table <- renderTable({
    req(data_combined())
    quick_stats(data_combined())
  })
  
  # Plot: platform_usage()
  output$platform_usage_comment <- renderPrint({
    req(data_combined())
    cat("Spotify usage on different platforms")
  })
  
  output$platform_usage_plot <- renderPlot({
    req(data_combined())
    platform_usage(data_combined())
  })
  
  # Kommentarblock
  output$listening_time_comment <- renderPrint({
    req(data_combined())
    cat("The total listening time of all your recorded streams in minutes")
  })
  
  # Textblock: Listening Time pro Jahr
  output$listening_time_text <- renderPrint({
    req(data_combined())
    txt <- capture.output({
      all_years <- sort(unique(substr(data_combined()$ts, 1, 4)))
      for (year in all_years) {
        print(paste(year, ":", listening_time(extract_year(year, data_combined())), "min"))
      }
      print(paste("All time:", listening_time(data_combined()), "min"))
    })
    cat(txt, sep = "\n")
  })
  
  #  Kommentarblock
  output$top_days_comment <- renderPrint({
    req(data_combined())
    cat("The days that you have listened the most music")
  })
  
  # Tabelle: Top Days Time
  output$top_days_table <- renderTable({
    req(data_combined())
    top_days_time(data_combined(), top_x = 10)
  })
  
  # Kommentarblock
  output$top_albums_comment <- renderPrint({
    req(data_combined())
    cat("Show the most listened albums")
  })
  
  # Tabelle: Top Albums
  output$top_albums_table <- renderTable({
    req(data_combined())
    top_albums(data_combined(), top_x = 10)
  })
  
  # Top Album
  output$top_album_comment <- renderPrint({
    req(data_combined())
    cat("Show the track listen counter for your favorite album\n")
  })
  
  # Top Album Name
  output$top_album_name_comment <- renderPrint({
    req(data_combined())
    top_album <- top_albums(data_combined(), top_x = 1)$Album_Name
    cat(top_album)
  })
  
  output$top_album_tracks_table <- renderTable({
    req(data_combined())
    top_album <- top_albums(data_combined(), top_x = 1)$Album_Name
    top_album_tracks(data_combined(), album_string = top_album, exact_search_bool = TRUE)
  })
  
  # Textblock: Kommentar zu track_per_year und Ausgabe der Tabelle
  output$track_per_year_comment <- renderPrint({
    req(data_combined())
    cat("Show the tracks that you have listened to the most often within one day\n")
  })
  
  output$track_per_year_table <- renderTable({
    req(data_combined())
    track_per_year(data_combined(), top_x = 10)
  })
  
  # Textblock: Kommentar zu top_artists und Ausgabe der Tabelle
  output$top_artists_comment <- renderPrint({
    req(data_combined())
    cat("Show your top artists by minutes played\n")
  })
  
  output$top_artists_table <- renderTable({
    req(data_combined())
    top_artists(data_combined(), top_x = 10)
  })
  
  # Textblock: Kommentar zu top_artist_name und Ausgabe der Tabelle
  output$top_artist_comment <- renderPrint({
    req(data_combined())
    cat("Show the top tracks from most played artist\n")
  })
  
  # Textblock: Kommentar zu top_artist_name 
  output$top_artist_name_comment <- renderPrint({
    req(data_combined())
    top_artist_name <- top_artists(data_combined(), 1)$Artist_Name
    cat(top_artist_name)
  })
  
  output$top_artist_tracks_table <- renderTable({
    req(data_combined())
    top_artist_name <- top_artists(data_combined(), 1)$Artist_Name
    artist_top_tracks(data_combined(), top_x = 10, artist_string = top_artist_name)
  })
  
  # Textblock: Kommentar zu top_featured_artists und Ausgabe der Tabelle
  output$top_featured_artists_comment <- renderPrint({
    req(data_combined())
    cat("Show the artists that appear most often in songs with features\n")
  })
  
  output$top_featured_artists_table <- renderTable({
    req(data_combined())
    top_featured_artists(data_combined(), top_x = 10)
  })
  
  # Textblock: Kommentar zu artist_month und Ausgabe der Tabelle
  output$artist_month_comment <- renderPrint({
    req(data_combined())
    cat("Show the top artists of each month by minutes played (recent year)\n")
  })
  
  # Textblock: Ausgabe recent year
  output$recent_year_comment <- renderPrint({
    req(data_combined())
    recent_year <- sort(unique(substr(data_combined()$ts, 1, 4)))[length(sort(unique(substr(data_combined()$ts, 1, 4)))) - 1]
    cat(recent_year)
  })
  
  output$artist_month_table <- renderTable({
    req(data_combined())
    recent_year <- sort(unique(substr(data_combined()$ts, 1, 4)))[length(sort(unique(substr(data_combined()$ts, 1, 4)))) - 1]
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    artist_month(recent_year_dataframe)
  })
  
  # Textblock: Kommentar zu time_artist_activity und Ausgabe der Tabelle
  output$time_artist_activity_comment <- renderPrint({
    req(data_combined())
    cat("Show the top artists by time of day\n")
  })
  
  output$time_artist_activity_table <- renderTable({
    req(data_combined())
    time_artist_activity(data_combined())
  })
  
  # Textblock: Kommentar zu top_tracks (All time und Recent Year) und Ausgabe der Tabellen
  output$top_tracks_comment <- renderPrint({
    req(data_combined())
    cat("Show the tracks that have been played most often (all time and recent year)\n")
  })
  
  output$top_tracks_table_all <- renderTable({
    req(data_combined())
    top_tracks(data_combined(), top_x = 10)
  })
  
  output$top_tracks_table_recent <- renderTable({
    req(data_combined())
    recent_year <- sort(unique(substr(data_combined()$ts, 1, 4)))[length(sort(unique(substr(data_combined()$ts, 1, 4)))) - 1]
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    top_tracks(recent_year_dataframe, top_x = 10)
  })
  
  # Textblock: Kommentar zu top_tracks (Incognito Mode) und Ausgabe der Tabelle
  output$top_tracks_incognito_comment <- renderPrint({
    req(data_combined())
    cat("Show most popular tracks with incognito mode enabled\n")
  })
  
  output$top_tracks_incognito_table <- renderTable({
    req(data_combined())
    top_tracks(incognito(data_combined()), top_x = 10)
  })
  
  # Tabelle: längste Lieder
  output$longest_tracks_comment <- renderPrint({
    req(data_combined())
    cat("Show the longest tracks that were listened")
  })
  
  output$longest_tracks_table <- renderTable({
    req(data_combined())
    longest_tracks(data_combined())
  })
  
  # Textblock: Kommentar zu time_year_plot und Ausgabe des Plots
  output$time_year_plot_comment <- renderPrint({
    req(data_combined())
    cat("Plot the listening time over the years\n")
  })
  
  output$time_year_plot <- renderPlot({
    req(data_combined())
    time_year_plot(data_combined())
  })
  
  # Textblock: Kommentar zu activity_time_plot und Ausgabe des Plots
  output$activity_time_plot_comment <- renderPrint({
    req(data_combined())
    cat("Plot with the minutes played per hour\n")
  })
  
  output$activity_time_plot <- renderPlot({
    req(data_combined())
    activity_time_plot(data_combined())
  })
  
  # Textblock: Kommentar zu country_plot und Ausgabe des Plots
  output$country_plot_comment <- renderPrint({
    req(data_combined())
    cat("Plot with the countries you have listened music in (connecting country)\n")
  })
  
  output$country_plot <- renderPlot({
    req(data_combined())
    country_plot(data_combined())
  })
  
  # Textblock: Kommentar zu top_tracks_over_time_plot und Ausgabe des Plots
  output$top_tracks_over_time_plot_comment <- renderPrint({
    req(data_combined())
    cat("Creates plot/pdf with your Top 100 songs over the years\n")
  })
  
  output$top_tracks_over_time_plot <- renderPlot({
    req(data_combined())
    top_tracks_over_time_plot(data_combined())
  })
  
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
    cat("Plot with the minutes played per month (all time and recent year)\n")
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
    recent_year <- sort(unique(substr(data_combined()$ts, 1, 4)))[length(sort(unique(substr(data_combined()$ts, 1, 4)))) - 1]
    recent_year_dataframe <- extract_year(recent_year, data_combined())
    activity_month_plot(recent_year_dataframe)
  })
  
  
  ### EXTENDED DATAFRAME
  
  # Textblock: Kommentar zu track_duration_plot und Ausgabe des Plots
  output$track_duration_plot_comment <- renderPrint({
    req(data_extended())
    cat("Plot with the distribution of song lengths of all streams\n")
  })
  
  output$track_duration_plot <- renderPlot({
    req(data_extended())
    track_duration_plot(data_extended(), TRUE)
  })
  
  # Textblock: Kommentar zu release_years_plot und Ausgabe des Plots
  output$release_years_plot_comment <- renderPrint({
    req(data_extended())
    cat("Plot with the release years of the songs streamed\n")
  })
  
  output$release_years_plot <- renderPlot({
    req(data_extended())
    release_years_plot(data_extended())
  })
  
  # Kommentar und Plot: Distribution of streams and follower number
  output$artist_follower_plot_comment <- renderPrint({
    req(data_extended())
    cat("Plot with the distribution of the streams and the follower number of the artist\n")
  })
  
  output$artist_follower_plot <- renderPlot({
    req(data_extended())
    artist_follower_plot(data_extended(), TRUE)
  })
  
  # Kommentar und Plot: Popularity of artist over amount of streams
  output$artist_popularity_plot_comment <- renderPrint({
    req(data_extended())
    cat("Plot with popularity of artist over amount of streams\n")
  })
  
  output$artist_popularity_plot <- renderPlot({
    req(data_extended())
    artist_popularity_plot(data_extended(), TRUE)
  })
  
  # Kommentar und Tabelle: Top Genres
  output$top_genres_comment <- renderPrint({
    req(data_extended())
    cat("Show the top genres\n")
  })
  
  output$top_genres_table <- renderTable({
    req(data_extended())
    top_genres(data_extended(), top_x = 10)
  })
  
  # Kommentar und Tabelle: Top Artists je Genre
  output$artist_genre_comment <- renderPrint({
    req(data_extended())
    cat("Show the favorite artist per genre\n")
  })
  
  #  Tabelle: Top Artist je Genre
  output$artist_genre_table <- renderTable({
    req(data_extended())
    top_artist_by_genre(data_extended(), top_x = 10)
  })
  
  # Kommentar und Tabelle: Top Genre by Month
  output$month_genre_activity_comment <- renderPrint({
    req(data_extended())
    cat("Show top genre by month (recent year)\n")
  })
  
  output$month_genre_activity_table <- renderTable({
    req(data_extended())
    recent_year <- sort(unique(substr(data_combined()$ts, 1, 4)))[length(sort(unique(substr(data_combined()$ts, 1, 4)))) - 1]
    recent_year_dataframe <- extract_year(recent_year, data_extended())
    month_genre_activity(recent_year_dataframe)
  })
  
  # Kommentar und Tabelle: Top Genre by Daytime
  output$time_genre_activity_comment <- renderPrint({
    req(data_extended())
    cat("Show top genre by daytime\n")
  })
  
  output$time_genre_activity_table <- renderTable({
    req(data_extended())
    time_genre_activity(data_extended())
  })
  
  # Kommentar und Textblock: Evaluate Music Taste
  output$evaluate_music_taste_comment <- renderPrint({
    req(data_extended())
    cat("Evaluate the exquisiteness of your music taste\n")
  })
  
  output$evaluate_music_taste_text <- renderPrint({
    req(data_extended())
    evaluate_music_taste(data_extended())
  })
  
  # Kommentar und Textblock: Artist Cluster Plot
  output$artist_cluster_plot_comment <- renderPrint({
    req(data_extended())
    cat("Plot Artist with follower count and average song length")
  })
  
  output$artist_cluster_plot_plot <- renderPlot({
    req(data_extended())
    artist_cluster_plot(data_extended())
  })
  
  
}

shinyApp(ui, server)
