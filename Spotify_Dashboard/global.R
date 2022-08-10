# load library for shiny app
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# load library for data wrangling
library(tidyverse)
library(scales)
library(glue)
library(plotly)
library(lubridate)
library(ggpubr)
library(GGally)

# Read data
spotify <- read.csv("spotify_dataset.csv")

# Drop NA
spotify <- na.omit(spotify)

# Cleansing data
spotify_clean <- spotify %>% 
  select(-c(Index, Highest.Charting.Position, Number.of.Times.Charted, Week.of.Highest.Charting, 
            Song.ID, Weeks.Charted, Chord, Genre)) %>%
  mutate(
    Streams = as.numeric(gsub(",","",Streams)),
    Artist = as.factor(Artist),
    Release.Date = ymd(Release.Date),
    followersp = Artist.Followers/Streams
  )

# artist_song <- spotify_clean %>%
#   filter(Artist == input$input_artist) %>%
#   select(Artist, Song.Name, Streams) %>%
#   arrange(-Streams) %>%
#   top_n(10)

# spotify_corr <- spotify %>%
#   select(Artist,Song.Name, Streams, Danceability, Energy, Loudness, Speechiness, Acousticness, Liveness, Tempo, Duration..ms.) %>%
#   # filter(input$input_feature) %>%
#   mutate(
#     Streams = as.numeric(gsub(",","",Streams)),
#     Duration_s = Duration..ms./1000,
#     Duration_m = round(Duration_s/60, 1)
#   )

