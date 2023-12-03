options(scipen = 99)

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(scales)
library(glue)
library(DT)
library(wordcloud2)
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(lubridate)
library(jsonlite)
library(knitr)
library(highcharter)
library(treemap)
library(viridis)


spotify <- read_csv("data.csv")
spotify2 <- read_csv("top10s.csv")

spotify_clean1 <- 
  spotify %>% 
  mutate(
    explicit = as.factor(explicit),
    mode = as.factor(mode),
    year = as.numeric(year),
    acousticness = as.numeric(acousticness),
    danceability = as.numeric(danceability),
    energy = as.numeric(energy),
    instrumentalness = as.numeric(instrumentalness),
    liveness = as.numeric(liveness),
    speechiness = as.numeric(speechiness),
    loudness = as.numeric(loudness),
    valence = as.numeric(valence)
    
  )

spotify_clean <- spotify_clean1 %>% arrange(desc(spotify_clean1$year))

# Read data
spotifyd <- read.csv("spotify_dataset.csv")

# Drop NA
spotifyd <- na.omit(spotifyd)

# Cleansing data
spotify_cleann <- spotifyd %>% 
  select(-c(Index, Highest.Charting.Position, Number.of.Times.Charted, Week.of.Highest.Charting, 
            Song.ID, Weeks.Charted, Chord, Genre)) %>%
  mutate(
    Streams = as.numeric(gsub(",","",Streams)),
    Artist = as.factor(Artist),
    Release.Date = ymd(Release.Date),
    followersp = Artist.Followers/Streams
  )

# READING JSON STREAMING HISTORY

streamHistory <- fromJSON("StreamingHistory0.json", flatten = TRUE)

# ADDING DATE AND TIMING 

mySpotify <- streamHistory %>% 
  as_tibble() %>% 
  mutate_at("endTime", ymd_hm) %>% 
  mutate(endTime = endTime - hours(6)) %>% 
  mutate(date = floor_date(endTime, "day") %>% as_date, seconds = msPlayed / 1000, minutes = seconds / 60)

spotifyy <- read.csv("C:/Users/kathi/Downloads/DV-Spotify/DV-Spotify/spotifyy.csv")