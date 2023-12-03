Sys.setenv(SPOTIFY_CLIENT_ID = "0944bf61019d4d18acc61fb1a2bd3b7f")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "fab71763c6d74e409658e02f2a7c517c")


readRenviron(".Renviron")
library(shiny)
library(spotifyr)
library(lubridate)
library(tidyverse)
library(plotly)
shinyServer(function(input, output) {
  
  #WORDCLOUD
  output$wordcloud2 <- renderWordcloud2({
    spotify2_clean <- 
      spotify2 %>% 
      mutate(
        genre = as.factor(genre),
        year = as.numeric(year),)
    
    
    spotify_by_genre <- spotify2_clean %>% 
      group_by(genre) %>%
      summarise(mean_popularity = mean(popularity)) %>% 
      ungroup()
    spotify_by_genre$mean_popularity <-round(spotify_by_genre$mean_popularity, digits=2)
    
    wordcloud2(data= spotify_by_genre, color = "random-dark", size=0.3, backgroundColor = "white")
  })
  
  #Trending Music in a Year
  output$plot_rank <- renderPlotly({
    data_agg2 <- spotify_clean %>%
      filter(year == input$year1) %>%
      select(name, popularity, artists) %>%
      arrange(desc(popularity)) %>%
      mutate(text = glue("popularity: {popularity}
                          artists: {artists}")) %>%
      head(15)
    
    plot_rank <- ggplot(data_agg2,
                        aes(x = popularity, y = reorder(name, popularity),
                            text = text)) +
      geom_col(aes(fill = popularity)) +
      scale_y_discrete(labels = wrap_format(30)) +
      scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
      scale_fill_gradient(low = "light blue", high = "dark blue") +
      labs(x = "Popularity (0-100)", y = NULL,
           title = glue("Top 15 Popular Songs {input$year1}")) +
      theme_minimal()
    
    ggplotly(plot_rank, tooltip = "text")
  })
  
  output$SOTY <- renderValueBox({
    
    data_agg2 <- spotify_clean %>%
      filter(year == input$year1) %>%
      arrange(desc(popularity)) %>%
      head(1)
    
    valueBox(value = "Song of the Year",
             subtitle = glue("{data_agg2$name} --- {data_agg2$artists}"),
             color = "green",
             icon = icon("trophy"))
  })
  
  output$Popularity <- renderValueBox({
    
    data_agg2 <- spotify_clean %>%
      filter(year == input$year1) %>%
      arrange(desc(popularity)) %>%
      head(1)
    
    valueBox(value = "Popularity Score",
             subtitle = glue("{data_agg2$popularity} / 100"),
             color = "green",
             icon = icon("spotify"))
  })
  
  
  ## CORRELATION PLOT
  output$plot_corr_explicit <- renderPlotly({
    data_agg_4 <- spotify_clean %>%
      filter(year == input$year2) %>%
      arrange(desc(popularity)) %>%
      head(input$head)
    
    plot_dist <- ggplot(data_agg_4,
                        aes_string(x = input$xlabel, y = input$ylabel)) +
      geom_jitter(aes(col = as.factor(explicit),
                      text = glue("{str_to_upper(explicit)}
                                  Title: {name}
                                  Artists: {artists}
                                  Popularity: {popularity}"))) +
      labs(x = input$xlabel, y = input$ylabel,
           title = glue("Distribution of Songs {input$year2}")) +
      guides(color = FALSE) +
      theme_minimal()
    
    if (input$trend == TRUE) {
      plot_dist <- plot_dist + geom_smooth()
    }
    
    ggplotly(plot_dist, tooltip = "text")
  })
  
  output$plot_corr_mode <- renderPlotly({
    data_agg_4 <- spotify_clean %>%
      filter(year == input$year2) %>%
      arrange(desc(popularity)) %>% 
      head(1000)
    
    plot_dist <- ggplot(data_agg_4, 
                        aes_string(x = input$xlabel, y = input$ylabel)) +
      geom_jitter(aes(col = as.factor(mode),
                      text = glue("{str_to_upper(mode)}
                                  Title: {name}
                                  Artists: {artists}
                                  Popularity: {popularity}"))) +
      labs(x = input$xlabel, y = input$ylabel, 
           title = glue("Distribution of Songs {input$year2}")) +
      guides(color = FALSE) + 
      theme_minimal()
    
    if (input$trend == TRUE) {
      plot_dist <- plot_dist + geom_smooth()
    }
    
    ggplotly(plot_dist, tooltip = "text")
  })
  
  
  
  
  #Data PAGE 
  output$data <- renderDataTable({
    DT::datatable(data = spotify, options = list(scrollX=T))
  })
  
  #Spotify Playlist Visualizer
  
  playlist_data <- eventReactive(input$submit, {
    audio_features <- get_playlist_audio_features(playlist_uris = input$uri) %>%
      drop_na(track.name) %>%
      group_by(added_by.id) %>%
      mutate(
        Artist = map_chr(track.artists, function(x) x$name[1]), 
        added_at = lubridate::as_datetime(added_at),
        track.duration = track.duration_ms/1000
      ) %>%
      rename(
        Acousticness = acousticness,
        `Added At` = added_at,
        `Added By` = added_by.id,
        `Danceability` = danceability,
        `Energy` = energy,
        Instrumentalness = instrumentalness,
        Liveness = liveness,
        Popularity = track.popularity,
        Speechiness = speechiness,
        Tempo = tempo,
        Valence = valence,
        `Duration (Sec)` = track.duration
      )
  })
  
  # Render plotly plot
  output$plot <- renderPlotly({
    # playlist data
    data <- playlist_data()
    
    # Set points color if color is selected
    if (input$color == "None") {
      base_plot <- data %>%
        ggplot()   
    } else {
      base_plot <- data %>%
        ggplot(aes(color = get(input$color))) # aes_string() doesn't seem to work with
      base_plot$labels$colour <- input$color    # col names with spaces so use get() instead
    }
    
    # if Added By is one of the selected axes, jitter points
    if (input$x == "Added By" | input$y == "Added By") {
      plot <- base_plot +
        geom_jitter(aes(x = get(input$x), y = get(input$y),
                        text = paste("Track: ", track.name, "\n",
                                     "Artist: ", Artist, "\n",
                                     "Album: ", track.album.name, "\n",
                                     input$x, ": ", get(input$x), "\n",
                                     input$y, ": ", get(input$y), "\n",
                                     sep="")),
                    size=2,
                    alpha=0.8) +
        labs(x = input$x, y = input$y,
             title = data$playlist_name[1]) +
        theme_minimal() 
    } else { # else, render regular scatter
      plot <- base_plot +
        geom_point(aes(x = get(input$x), y = get(input$y),
                       text = paste("Track: ", track.name, "\n",
                                    "Artist: ", Artist, "\n",
                                    "Album: ", track.album.name, "\n",
                                    "Added By: ", `Added By`, "\n",
                                    input$x, ": ", get(input$x), "\n",
                                    input$y, ": ", get(input$y), "\n",
                                    sep="")),
                   size = 2,
                   alpha = 0.8) +
        labs(x = input$x, y = input$y,
             title = data$playlist_name[1]) +
        theme_minimal()
      # if trendline is checked add smooth
      if (input$smooth == TRUE) {
        plot <- plot +
          geom_smooth(aes(x = get(input$x), y = get(input$y), color=NULL), se = FALSE)
      }
    }
    
    ggplotly(plot, tooltip = "text")
  })
  
  ## ----- PLOT 1
  output$plot1_song <- renderPlotly({
    
    spotify_top <- spotify_cleann %>% 
      group_by(Artist) %>% 
      summarise(sum_streams = sum(Streams)) %>% 
      ungroup() %>% 
      arrange(-sum_streams) %>% 
      top_n(input$obs)
    
    spotify_top1 <- spotify_top %>% 
      mutate(
        label = glue(
          "Artist:{Artist}
    Total Streams: {comma(sum_streams)}"
        )
      )
    
    plot1 <- ggplot(data=spotify_top1, aes(x = sum_streams,
                                           y = reorder(Artist,sum_streams), 
                                           text = label)) +
      geom_col(aes(fill = sum_streams),show.legend = F) +
      scale_fill_gradient(low="light green", high="dark green") +
      labs(title = "Top Artist with the most listeners 2020-2021",
           x = "Total Streams",
           y = NULL) +
      scale_x_continuous(labels = comma) +
      theme_light() +
      theme(plot.title = element_text(size=24, hjust = 0.5, 
                                      face = 'bold', family = "Helvetica"))
    
    ggplotly(plot1, tooltip = "text")
  })
  
  ## ----- PLOT 2 
  output$plot2_artist <- renderPlotly({
    
    followers <- spotify_cleann %>%
      filter(!grepl(',', Artist)) %>%
      group_by(Artist) %>%
      summarise(artist_followers= max(Artist.Followers)) %>%
      ungroup() %>%
      arrange(-artist_followers) %>%
      top_n(input$obs2)
    
    followers1 <- followers %>%
      mutate(
        label = glue(
          "Followers: {comma(artist_followers)}")
      )
    
    plot2 <- ggplot(data=followers1, aes(x = artist_followers,
                                         y = reorder(
                                           Artist,artist_followers), text = label)) +
      geom_col(aes(fill = artist_followers),show.legend = F) +
      scale_fill_gradient(low="orange", high="dark red") +
      labs(title = "Top Artist Followers on Spotify 2020-2021",
           x = "Total Follower",
           y = NULL) +
      scale_x_continuous(labels = comma) +
      theme_light()+
      theme(plot.title = element_text(size=18, hjust = 0.5, 
                                      face = 'bold', family = "Helvetica"))
    
    ggplotly(plot2, tooltip = "text")
  })
  
  ## ----- PLOT 3
  output$plot3_artist_song <- renderPlotly({
    
    artist_song <- spotify_cleann %>%
      filter(Artist == input$input_artist) %>%
      select(Artist, Song.Name, Streams) %>%
      arrange(-Streams) %>%
      top_n(10)
    
    artist_song1 <- artist_song %>%
      mutate(
        label = glue(
          "Streams: {comma(Streams)}"
        )
      )
    
    plot3 <- ggplot(data=artist_song1, aes(x = Streams,
                                           y = reorder(Song.Name,Streams), text = label)) +
      geom_segment(aes(x=0,
                       xend=Streams, 
                       y=reorder(Song.Name,Streams), 
                       yend=reorder(Song.Name,Streams)), 
                   color="black") +
      geom_point(color = 'green') +
      scale_x_continuous(labels = comma) +
      scale_fill_gradient(low="light blue", high="dark blue") +
      labs(title = "Top Songs by Artist",
           x = "Total Streams",
           y = NULL) +
      theme_grey() +
      theme(plot.title = element_text(size=18, hjust = 0.5, 
                                      face = 'bold', family = "Helvetica"))
    
    ggplotly(plot3, tooltip = "text")
  })
  
  ## ----- Creator
  {output$mySite <- renderUI({
    tags$a(href = input$website, input$website)
  })
  }
  
  ## -- Plot 5 (top)
  output$plot5_corr <- renderPlot({
    
    spotify_corr <- spotify %>%
      select(Artist,Song.Name, Streams, Danceability, Energy, Loudness, Speechiness, Acousticness, Liveness, Tempo, Duration..ms.) %>%
      # filter(input$input_feature) %>%
      mutate(
        Streams = as.numeric(gsub(",","",Streams)),
        Duration_s = Duration..ms./1000,
        Duration_m = round(Duration_s/60, 1)
      )
    
    spotify_corr2 <- spotify_corr %>%
      select(-c(Duration..ms., Duration_s))
    
    ggcorr(spotify_corr2, label = TRUE,
           label_size = 2.9, hjust = 1, layout.exp = 2)
  })
  
  ## ----- Plot 4 (bottom)
  output$plot4_analysis <- renderPlotly({
    
    spotify_corr <- spotify %>% 
      select(Artist,Song.Name, Streams, Danceability, Energy, Loudness, Speechiness, Acousticness, Liveness, Tempo, Duration..ms.) %>% 
      mutate(
        Streams = as.numeric(gsub(",","",Streams)),
        Duration_s = Duration..ms./1000,
        Duration_m = round(Duration_s/60, 1)
      )
    
    spotify_corr1 <- spotify_corr %>%
      mutate(
        label = glue(
          "{Artist}
    {Song.Name}
    Streams: {comma(Streams)}
    Tempo: {Tempo}"
        )
      )
    
    plot_corr1 <- ggplot(data = spotify_corr1, aes(x = Tempo, y = Streams))+ 
      geom_point(color='blue', alpha = 0.4, aes(text=label)) +
      geom_smooth(method='lm', se = FALSE, color = 'red',
                  formula = y~x) +
      scale_y_continuous(labels = comma) +
      theme(plot.title = element_text(size=18, hjust = 0.5, 
                                      face = 'bold', family = "Helvetica"))
    
    ggplotly(plot_corr1 ,tooltip = "text")
  })
  
  ## ----- Plot 6 (bottom)
  output$plot6_analysis <- renderPlotly({
    
    spotify_corr <- spotify %>% 
      select(Artist,Song.Name, Streams, Danceability, Energy, Loudness, Speechiness, Acousticness, Liveness, Tempo, Duration..ms.) %>% 
      mutate(
        Streams = as.numeric(gsub(",","",Streams)),
        Duration_s = Duration..ms./1000,
        Duration_m = round(Duration_s/60, 1)
      )
    
    spotify_corr2 <- spotify_corr %>%
      mutate(
        label = glue(
          "{Artist}
    {Song.Name}
    Streams: {comma(Streams)}
    Danceability: {Danceability}"
        )
      )
    
    plot_corr2 <- ggplot(data = spotify_corr2, aes(x = Danceability, y = Streams))+ 
      geom_point(color='blue', alpha = 0.4, aes(text=label)) +
      geom_smooth(method='lm', se = FALSE, color = 'red',
                  formula = y~x) +
      scale_y_continuous(labels = comma) +
      theme(plot.title = element_text(size=18, hjust = 0.5, 
                                      face = 'bold', family = "Helvetica"))
    
    ggplotly(plot_corr2 ,tooltip = "text")
  })
  
  ## ----- Plot 7 (bottom)
  output$plot7_analysis <- renderPlotly({
    
    spotify_corr <- spotify %>% 
      select(Artist,Song.Name, Streams, Danceability, Energy, Loudness, Speechiness, Acousticness, Liveness, Tempo, Duration..ms.) %>% 
      mutate(
        Streams = as.numeric(gsub(",","",Streams)),
        Duration_s = Duration..ms./1000,
        Duration_m = round(Duration_s/60, 1)
      )
    
    spotify_corr3 <- spotify_corr %>%
      mutate(
        label = glue(
          "{Artist}
    {Song.Name}
    Streams: {comma(Streams)}
    Speechiness: {Speechiness}"
        )
      )
    
    plot_corr3 <- ggplot(data = spotify_corr3, aes(x = Speechiness, y = Streams))+ 
      geom_point(color='blue', alpha = 0.4, aes(text=label)) +
      geom_smooth(method='lm', se = FALSE, color = 'red',
                  formula = y~x) +
      scale_y_continuous(labels = comma) +
      theme(plot.title = element_text(size=18, hjust = 0.5, 
                                      face = 'bold', family = "Helvetica"))
    
    ggplotly(plot_corr3 ,tooltip = "text")
  })
  
  hoursDay <- mySpotify %>% 
    filter(date >= "2019-01-01") %>% 
    group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE))%>% 
    summarize(minutesListened = sum(minutes))
  
  output$personal1 <- renderPlotly({
    hoursDay %>% 
      group_by(weekday, hour) %>% 
      summarize(minutes = sum(minutesListened)) %>% 
      ggplot(aes(x = hour, weekday, fill = minutes)) + 
      geom_tile() + 
      scale_fill_gradient(low = "yellow", high = "red") +
      labs(x= "Time of the day", y= "Weekday") + 
      ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Weekly activity from 0 to 24 hours") 
    ggplotly()
  })
  
  output$personal2 <- renderPlotly({
    timeDay <- mySpotify %>% 
      filter(date >= "2020-01-01") %>% 
      group_by(date, hour = hour(endTime)) %>% 
      summarize(minutesListened = sum(minutes)) %>% 
      ggplot(aes(x = hour, y = date, fill = minutesListened)) + 
      geom_tile() + 
      labs(x= "Time of the day", y= "Date") + 
      ggtitle("When has there been more playback activity on my Spotify?", "Activity by date and time of day") +
      scale_fill_gradient(low = "yellow", high = "red")
    timeDay
    ggplotly()
  })
  
  output$personal3 <- renderPlotly({
    weekDay <- hoursDay %>% 
      group_by(weekday, hour) %>% 
      summarize(minutes = sum(minutesListened)) %>% 
      ggplot(aes(x = hour, y = minutes, color = weekday)) + 
      geom_line() +
      labs(x= "Time of the day", y= "Minutes of music playback") + 
      ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Line chart - Weekly activity from 0 to 24 hours") 
    weekDay
    ggplotly()  
  })
  
  output$personal4 <- renderPlotly({
    streamingHours <- mySpotify %>% 
      filter(date >= "2020-01-01") %>% 
      group_by(date) %>% 
      group_by(date = floor_date(date, "week")) %>%
      summarize(hours = sum(minutes) / 60) %>% 
      arrange(date) %>% 
      ggplot(aes(x = date, y = hours)) + 
      geom_col(aes(fill = hours)) +
      scale_fill_gradient(low = "yellow", high = "red") + 
      labs(x= "Date", y= "Hours of music playback") + 
      ggtitle("On what dates I've listened to more or less music on Spotify?", "Playback activity per week")
    streamingHours
    ggplotly()
  })
  
  output$personal5 <- renderPlotly({
    hoursDay %>% 
      ggplot(aes(x = hour, y = minutesListened, group = date)) + 
      geom_col(fill = "#ff6600") +
      labs(x= "Time of the day", y= "Minutes of music playback") + 
      ggtitle("What time of day I've listened to the most music on Spotify?", "Activity from 0 to 24 hours") 
    ggplotly()  
  })
  
  output$personal6 <- renderPlotly({
    minutesMostListened <- mySpotify %>% 
      filter(date >= "2020-01-01") %>% 
      group_by(artistName) %>% 
      summarize(minutesListened = sum(minutes)) %>% 
      filter(minutesListened >= 180) %>%
      ggplot(aes(x = artistName, y = minutesListened)) + 
      geom_col(aes(fill = minutesListened)) +
      scale_fill_gradient(low = "yellow", high = "red") + 
      labs(x= "Artist", y= "Minutes of music playback") + 
      ggtitle("What were the most listened artists on my Spotify?", "> 3 hours listened") +
      theme(axis.text.x = element_text(angle = 90))
    minutesMostListened
    ggplotly()
  })
  
  output$emotion <- renderHighchart({
    artist_name <- "Drake"
    artist <- get_artist_audio_features(artist_name)
    track_names <- artist$track_name
    
    scatter_plot <- hchart(artist, "scatter", hcaes(x = energy, y = valence, group = track_names)) %>%
      hc_title(text = "Sentiment Analysis of Artist's Songs", align = "center") %>%
      hc_xAxis(title = list(text = "Valence")) %>%
      hc_yAxis(title = list(text = "Energy"))
    
    scatter_plot <- scatter_plot %>%
      hc_plotOptions(
        scatter = list(
          marker = list(
            radius = 5
          )
        )
      ) %>%
      hc_xAxis(
        plotLines = list(
          list(
            color = "black",
            width = 1,
            value = 0.5,
            zIndex = 1
          )
        )
      ) %>%
      hc_yAxis(
        plotLines = list(
          list(
            color = "black",
            width = 1,
            value = 0.5,
            zIndex = 4
          )
        )
      ) %>%
      hc_add_annotation(xvalue = 0, yvalue = 0.5, title = list(text = "Quadrant 1"))
    
    scatter_plot
  })
  
  
  spotifyy <- read.csv("C:/Users/kathi/Downloads/DV-Spotify/DV-Spotify/spotifyy.csv")
  
  output$genre1 <- renderPlot({
    spotifyy <- read.csv("C:/Users/kathi/Downloads/DV-Spotify/DV-Spotify/spotifyy.csv")
    
    top_genre <- spotifyy %>% 
      select(playlist_genre, track_artist, track_popularity) %>% 
      group_by(playlist_genre, track_artist) %>% 
      summarise(n = n()) %>% 
      top_n(10, n)
    
    tm <- treemap(top_genre, index = c("playlist_genre", "track_artist"), vSize = "n", 
                  vColor = 'playlist_genre', palette = viridis(7), title = "Top 10 Artists by Genre")
    
    return(tm)
  })
  
  output$genre2 <- renderPlot({
    spotifyy <- read.csv("C:/Users/kathi/Downloads/DV-Spotify/DV-Spotify/spotifyy.csv")
    
    top <- spotifyy %>% select(playlist_genre, playlist_subgenre, track_popularity) %>% 
      group_by(playlist_genre,playlist_subgenre) %>% 
      summarise(n = n()) %>% 
      top_n(3, n)
    tm <- treemap(top, index = c("playlist_genre", "playlist_subgenre"), vSize = "n", 
                  vColor = 'playlist_genre', palette =  viridis(7), ,title="Top 3 Subgenres within each Genre" )
    
  })
  
  output$genre3 <- renderPlotly({
    spotifyy <- read.csv("C:/Users/kathi/Downloads/DV-Spotify/DV-Spotify/spotifyy.csv")
    x<- ggplot(spotifyy) +
      geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
      geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
      geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
      geom_density(aes(acousticness, fill ="acousticness", alpha = 0.1)) + 
      geom_density(aes(speechiness, fill ="speechiness", alpha = 0.1)) + 
      geom_density(aes(liveness, fill ="liveness", alpha = 0.1)) + 
      scale_x_continuous(name = "Energy, Danceability, Valence, Acousticness, Speechiness, Liveness") +
      scale_y_continuous(name = "Density") +
      theme_bw() +
      ggtitle("Density Plots") +
      theme(plot.title = element_text(size = 13, face = "bold", colour = "#13833c"),
            text = element_text(size = 11,colour = "#13833c")) +
      theme(panel.background = element_rect(fill = "#ebebeb")) +
      theme(plot.background = element_rect(fill = "#ebebeb")) +
      theme(legend.background = element_rect(fill = "#ebebeb"))+
      theme(axis.text.x = element_text(colour = "#13833c",size = 9))+
      theme(axis.text.y = element_text(colour = "#13833c",size = 9))
    
    ggplotly(x)
  })
  
  output$genre4 <- renderPlot({
    # Box Plot of genre by valence
    # Latin genre has a higher valence than others
    ggplot(spotifyy, aes(x=valence, y=playlist_genre)) +
      geom_boxplot(color="black", fill="darkgreen")  +
      scale_x_continuous(name = "Valence") +
      scale_y_discrete(name = "Genre") +
      theme_bw() +
      ggtitle("Valence of Genre") +
      
      theme(plot.title = element_text(size = 14, face = "bold", colour = "darkgreen"),
            text = element_text(size = 11,colour = "darkgreen")) +
      theme(legend.title=element_blank()) +
      scale_fill_brewer(palette="Accent") + 
      theme(axis.text.x = element_text(colour = "darkgreen"))+
      theme(panel.background = element_rect(fill = "#ebebeb"), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      theme(plot.background = element_rect(fill = "#ebebeb")) +
      theme(legend.background = element_rect(fill = "#ebebeb"))+
      theme(axis.text.y = element_text(colour = "darkgreen"))
    
  })
  
  output$genre5 <- renderPlot({
    ### Proportion of Genres
    custom <- viridis::plasma(n=15)
    
    spoify_genre <- spotifyy %>% 
      group_by(playlist_genre) %>% 
      summarise(Total_number_of_tracks = length(playlist_genre))
    
    ggplot(spoify_genre, aes(x="", y="", fill=playlist_genre)) + 
      geom_bar(width = 1, stat = "identity") + 
      coord_polar("y", start=0) + 
      geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
                position = position_stack(vjust = 0.5), colour = "#13833c")+
      ggtitle("Proportion of Genres") +
      theme(plot.title = element_text(size = 13, face = "bold", colour = "#13833c"),
            text = element_text(size = 11,colour = "#13833c")) +
      theme(panel.background = element_rect(fill = "#ebebeb")) +
      theme(plot.background = element_rect(fill = "#ebebeb")) +
      theme(legend.background = element_rect(fill = "#ebebeb"))+
      theme(axis.text.x = element_text(colour = "#13833c",size = 9))+
      theme(axis.text.y = element_text(colour = "#13833c",size = 9))
    
  })
  
  output$audio_feature_plot <- renderPlotly({
    req(input$feature_dropdown, input$artist_dropdown)  # Ensure a feature is selected
    artist_name <- input$artist_dropdown
    artist_info <- search_spotify(artist_name, type="artist",limit=1)
    artist_id <- artist_info$id
    artist_albums <- get_artist_albums(artist_id)
    
    feature_data <- data.frame(
      Album = character(),
      Value = double(),
      stringsAsFactors = FALSE
    )
    
    for (album_id in artist_albums$id) {
      album_tracks <- get_album_tracks(album_id)
      track_ids <- album_tracks$id
      audio_features_data <- get_track_audio_features(track_ids)
      
      # Filter out non-numeric values for the selected feature
      numeric_values <- suppressWarnings(as.numeric(audio_features_data[[input$feature_dropdown]]))
      numeric_values <- numeric_values[!is.na(numeric_values)]
      
      # Check if there are numeric values
      if (length(numeric_values) > 0) {
        avg_value <- mean(numeric_values)
        album_name <- artist_albums$name[artist_albums$id == album_id]
        feature_data <- rbind(feature_data, data.frame(Album = album_name, Value = avg_value))
      } else {
        cat("No numeric values for", input$feature_dropdown, "in album", album_id, "\n")
      }
    }
    
    # Check if there is any data to plot
    if (nrow(feature_data) > 0) {
      plot_ly(feature_data, x = ~Album, y = ~Value, type = "bar", text = ~Value, textposition = "outside", marker = list(color = "blue")) %>%
        layout(
          title = paste("Average", str_to_title(input$feature_dropdown), "for Albums by", artist_name),
          xaxis = list(title = "Album"),
          yaxis = list(title = paste("Average", str_to_title(input$feature_dropdown)))
        )
    } else {
      # If no data, display a message or an empty plot
      plot_ly() %>%
        layout(title = "No valid data for the selected feature")
    }
  })
  
  
})

