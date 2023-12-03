vars_list <- list(
  "Acousticness", "Added At", "Added By", "Danceability",
  "Energy", "Instrumentalness", "Liveness", "Popularity", "Speechiness",
  "Tempo", "Valence", "Duration (Sec)"
)

audio_features <- c("acousticness","danceability",
                    "energy", "instrumentalness", "liveness", "speechiness",
                    "tempo", "valence", "duration_ms")

dashboardPage(
  
  skin = "green",
  
  # HEADER
  dashboardHeader(title = "Spotify Analysis"),
  
  # SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("music")),
      menuItem(text = "Trending Music in a Year", tabName = "stats1", icon = icon("chart-line")),
      menuItem(text = "Songs Correlation on Spotify", tabName = "stats2", icon = icon("chart-line")),
      menuItem(text = "Playlist Analyzer", tabName = "stats4", icon = icon("chart-line")),
      menuItem(text = "Audio Features Analysis", tabName = "stats12", icon = icon("chart-line")),
      menuItem(text = "Artist with most listeners", tabName = "stats5", icon = icon("chart-line")),
      menuItem(text = "Top Songs by Artist", tabName = "stats7", icon = icon("chart-line")),
      menuItem(text = "Artist with most followers", tabName = "stats6", icon = icon("chart-line")),
      menuItem(text = "Genre based on Popularity", tabName = "stats3", icon = icon("chart-line")),
      menuItem(text = "User Data Analysis I", tabName = "stats8", icon = icon("chart-line")),
      menuItem(text = "User Data Analysis II", tabName = "stats9", icon = icon("chart-line")),
      menuItem(text = "Sentimental Analysis of Songs", tabName = "stats10", icon = icon("chart-line")),
      menuItem(text = "Genre Based Analysis", tabName = "stats11", icon = icon("chart-line")),
      menuItem(text = "Data", tabName = "tab_data", icon = icon("table")),
      menuItem(text = "About Us", tabName = "User_Profile", icon = icon("eye"))
    )
  ),
  
   # BODY
   dashboardBody(
     tabItems(
       
       # PAGE 1 #
       
       tabItem(tabName = "Introduction",
               fluidPage(
                 h2(tags$b("Spotify: Listening is Everything")),
                 div(style = "text-align:justify",
                     p("Spotify, a game-changer in music streaming since 2008, grants immediate access to a vast library of music and podcasts. Its user-friendly interface and legal framework make it effortlessly enjoyable across devices, from computers to cars, thanks to Spotify Connect. The platform's mission is to unlock human creativity by supporting a million artists to make a living through their art, offering billions of fans the chance to indulge in and be inspired by a world of creative possibilities."),
                     br()
                     )
               ),
               
              
               
               fluidRow(
                 valueBox("80 Million", "Tracks", icon = icon("music"), color = "red"),
                 valueBox("4.7 Million", "Podcasts", icon = icon("headphones"), color = "purple"),
                 valueBox("183 Millions" , "Markets", icon = icon("globe-asia"), color = "blue"),
                 valueBox("456 Million", "Active Users", icon = icon("play-circle"), color = "green"),
                 valueBox("195 Million", "Subscribers", icon = icon("user"), color = "orange"),
                 valueBox("4 Billion+", "Playlists", icon = icon("record-vinyl"), color = "fuchsia")
               ),
               
               br(),
               
       ),
       
       
       # PAGE 2 #
       
       tabItem(tabName = "stats1",
               
               fluidPage(
                 
                 box(width = 8,
                     solidHeader = T,
                     title = tags$b("Trending Music in a year"),
                     plotlyOutput("plot_rank")),
                 box(width = 4,
                     solidHeader = T,
                     height = 150,
                     background = "purple",
                     selectInput(inputId = "year1",
                                 label = "Select Year",
                                 choices = unique(spotify_clean$year),)
                 ),
                 
                 valueBoxOutput("SOTY", width = 4),
                 valueBoxOutput("Popularity",width = 4),
                 br()
               ),
       ),
       
       # PAGE 3 #
       
       tabItem(tabName = "stats2",
               #correlation plot
               fluidPage(
                 tabBox(width = 9,
                        title = tags$b("Songs Correlation on Spotify"),
                        side = "right",
                        tabPanel(tags$b("by Mode"),
                                 plotlyOutput("plot_corr_mode", height=480)
                        ),
                        tabPanel(tags$b("by Explicitness"),
                                 plotlyOutput("plot_corr_explicit", height=480)
                        )
                 ),
                 box(width = 3,
                     solidHeader = T,
                     background = "purple",
                     selectInput(inputId = "year2",
                                 label = "Select Year",
                                 choices = unique(spotify_clean$year))
                 ),
                 box(width = 3,
                     solidHeader = T,
                     background = "green",
                     selectInput(inputId = "xlabel",
                                 label = "Select X Axis",
                                 choices = spotify_clean %>%
                                   select('energy', 'valence', 'acousticness', 'danceability',
                                          'instrumentalness', 'liveness', 'loudness', 'speechiness', 'tempo') %>%
                                   names()),
                     selectInput(inputId = "ylabel",
                                 label = "Select Y Axis",
                                 choices = spotify_clean %>%
                                   select('loudness', 'valence', 'acousticness', 'danceability',
                                          'instrumentalness', 'liveness', 'energy', 'speechiness', 'tempo') %>%
                                   names())
                 ),
                 box(tags$b("Choose whether to display trend"),
                     width = 3,
                     solidHeader = T,
                     background = "green",
                     checkboxInput("trend", "Display trend", FALSE)
                 ),
                 box(width = 3,
                     solidHeader = T,
                     background = "purple",
                     sliderInput("head", "Number of data", min = 100, max = 3000, value = 1000)
                 )
               )
       ),
       
       # PAGE 4 #
       
       tabItem(tabName = "stats3",
               fluidPage( 
                 h2(tags$b("Genre based on Popularity on Spotify")),
                 wordcloud2Output("wordcloud2"),
               )
       ),
       
       
       # PAGE 5 #
       
       tabItem(tabName = "stats4",
               fluidPage(
                 
                  # Application title
                  titlePanel("Spotify Playlist Visualizer"),
                     
                  textInput("uri", "Input Playlist ID (At the end of playlist URL): "),
                  actionButton("submit", "Submit"),
                     
                     
                  sidebarLayout(
                  sidebarPanel(
                  selectInput("x", "X-axis:", vars_list),
                  selectInput("y", "Y-axis:", vars_list),
                         selectInput("color", "Color:", c("None", vars_list)),
                         checkboxInput("smooth", "Trendline", value = FALSE)
                  ),
                       
                  mainPanel(plotlyOutput("plot"))
              )),
       ),
       
       # PAGE 6 #
       
       tabItem(tabName = "stats5",
               
               ## ----- ROW 1 : PLOT 1
               fluidRow(
                 box(width = 12,
                     plotlyOutput(outputId = "plot1_song"))
               ),
               
               ## ----- ROW 2 : SLIDE INPUT
               fluidRow(
                 box(width = 12,
                     chooseSliderSkin("Big"),
                     sliderInput("obs", "Choose a number : ",
                                 min = 1, max = 20, value = 10
                     ))
               ),
               
               ## ----- ROW 3 : INFO BOX
               fluidRow(
                 infoBox(width = 4,
                         color = "black",
                         title = "Total Song Count",
                         icon = icon("spotify"),
                         value = nrow(spotify_clean)),
                 infoBox(width = 4,
                         color = "green",
                         title = "Total Spotify User",
                         icon = icon("list"),
                         value = "299,000,000+"),
                 infoBox(width = 4,
                         color = "yellow",
                         title = "Total Artist Count",
                         icon = icon("book"),
                         value = length(unique(spotify_clean$Artist)))
               )
       ),
       
       # PAGE 7 #
       tabItem(tabName = "stats7",
               ## ----- ROW 1 : PLOT 1
               fluidRow(
                 box(width = 12,
                     plotlyOutput(outputId = "plot3_artist_song"))
               ),
               
               # ----- ROW 2 : INPUT
               fluidRow(
                 box(width = 12,
                     selectInput(inputId = "input_artist",
                                 label = "Choose Artist",
                                 selected = "Eminem",
                                 choices = sort(unique(spotify_cleann$Artist))
                     )
                 )
               )
       ),
       
       # PAGE 8 #
               
       tabItem(tabName = "stats6",
               ## ----- ROW 1 : PLOT 2
               fluidRow(
                 box(width = 12,
                     plotlyOutput(outputId = "plot2_artist"))
               ),
               
               ## ----- ROW 2 : SLIDE INPUT
               fluidRow(
                 box(width = 12,
                     chooseSliderSkin("Big"),
                     sliderInput("obs2", "Choose a number : ",
                                 min = 1, max = 20, value = 10
                     ))
               ),
       ),
       
       # PAGE 9 #
       
       tabItem(tabName = "stats8",
               fluidRow(
                 box(width = 12,
                     plotlyOutput("personal1"))
               ),
               fluidRow(
                 box(width = 12,
                     plotlyOutput("personal2"))
               ),
               fluidRow(
                 box(width = 12,
                     plotlyOutput("personal3"))
               ),
       ),
       
       # PAGE 10 #
       
       tabItem(tabName = "stats9",
               fluidRow(
                 box(width = 12,
                     plotlyOutput("personal4"))
               ),
               fluidRow(
                 box(width = 12,
                     plotlyOutput("personal5"))
               ),
               fluidRow(
                 box(width = 12,
                     plotlyOutput("personal6"))
               ),
       ),
       
       # PAGE 11 #
       
       tabItem(tabName = "stats10",
               fluidRow(
                 box(width = 25,
                     highchartOutput("emotion"))
               )
       ),
       
       # PAGE 12 #
       
       tabItem(tabName = "stats11",
               fluidRow(
                 box(width = 12,
                     plotOutput("genre1"))
               ),
               fluidRow(
                 box(width = 12,
                     plotOutput("genre2"))
               ),
               fluidRow(
                 box(width = 12,
                     plotlyOutput("genre3"))
               ),
               fluidRow(
                 box(width = 12,
                     plotOutput("genre4"))
               ),
               fluidRow(
                 box(width = 12,
                     plotOutput("genre5"))
               ),
       ),
       
       # PAGE 13 #
       tabItem(tabName = "stats12",
               fluidRow(
                 box(width = 12,
                     selectInput("artist_dropdown", "Select Artist", choices = unique(spotify_cleann$Artist)),
                     selectInput("feature_dropdown", "Select Audio Feature", choices = audio_features),
                     plotlyOutput("audio_feature_plot")
                 )
               )
       ),
       
       
       # Data
       
       tabItem(tabName = "tab_data", 
               fluidPage(
                 h2(tags$b("Understanding the Data")),
                 br(),
                 dataTableOutput(outputId = "data"),
                 br(),
                 div(style = "text-align:justify", 
                     p("This dashboard uses a dataset from ", tags$a(href="https://www.kaggle.com/datasets/ektanegi/spotifydata-19212020", "kaggle"), " which is cleaned"),
                     
                     br()
                 )
               )
       ),
       
       # Author
       
       tabItem(tabName = "User_Profile", 
               fluidPage(
                 h2(tags$b("Team Details")),
                 br(),
                 br(),
                 div(style = "text-align:justify",
                     p("This project is a collaborative effort by the following team members for the J component submission of our subject Data Visualization(CSE3020). You can connect with us through the provided LinkedIn profiles, and please feel free to reach out:)")),
                 
                 br(),
                 
                 # Team Member 1
                 p(
                   "1: Kathiresan PL",
                   br(),
                   "   Registration Number: 20BCE2400",
                   br(),
                   "   LinkedIn: ", tags$a(href = "https://www.linkedin.com/in/kathiresan07/", "Kathiresan PL")
                 ),
                 
                 br(),
                 
                 # Team Member 2
                 p(
                   "2: Takshu Yadav",
                   br(),
                   "   Registration Number: 20BCE2796"
                 ),
                 
                 br(),
                 
                 # Team Member 3
                 p(
                   "3: Ishika Garg",
                   br(),
                   "   Registration Number: 20BCE2793"
                 ),
                 
                 br(),
                 
                 # Team Member 4
                 p(
                   "4: Annanya Mangla",
                   br(),
                   "   Registration Number: 20BCE0700"
                 ),
                 
                 br(),
                 
                 # Team Member 5
                 p(
                   "5: Sisira Reddy",
                   br(),
                   "   Registration Number: 20BCE2448"
                 )
               )
       )
       
       
       
       )
     )
  )





