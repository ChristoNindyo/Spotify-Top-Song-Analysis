shinyServer(function(input, output, session){
  
  ## ----- PLOT 1
  output$plot1_song <- renderPlotly({
    
    spotify_top <- spotify_clean %>% 
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

    followers <- spotify_clean %>%
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

    artist_song <- spotify_clean %>%
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
  
})
