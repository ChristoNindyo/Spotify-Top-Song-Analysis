dashboardPage(
  
  skin = "green",
  
  # ----- HEADER
  dashboardHeader(
    title = "Spotify Analysis",
    dropdownMenu(messageItem(
                   from = "Creator",
                   message = "What can we get from Spotify data ?",
                   icon = icon("question"),
                 ),
                 messageItem(
                   from = "Spotify",
                   message = "Dataset: Top 200 Spotify Songs 2021-2022",
                   icon = icon("laptop"),
                   time = "2022-August-3"
                 )
    )
  ),
  
  # ----- SIDEBAR
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Song", tabName = 'page1', icon = icon("spotify")),
      menuItem("Artist", tabName = 'page2', icon = icon("music")),
      menuItem("Analysis", tabName = 'page3', icon = icon("code")),
      menuItem("Creator", tabName = 'page4', icon = icon("person"))
    )
  ),
  
  # ----- BODY
  dashboardBody(
    
    tabItems(
      # ----- PAGE 1 -----
      tabItem(tabName = "page1",
              
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

      # ----- PAGE 2 -----
      tabItem(tabName = "page2",
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
              
              ## ----- ROW 3 : PLOT 2
              fluidRow(
                box(width = 12,
                    plotlyOutput(outputId = "plot3_artist_song"))
              ),

              # ----- ROW 4 ; INPUT
              fluidRow(
                box(width = 12,
                    selectInput(inputId = "input_artist",
                                label = "Choose Artist",
                                selected = "Eminem",
                                choices = sort(unique(spotify_clean$Artist))
                    )
                  )
              )
    ),
    # ----- PAGE 3 -----
    tabItem(tabName = "page3",
            
            ## ----- ROW 1 : PLOT 
            fluidRow(
              box(width = 12,
                  plotOutput(outputId = "plot5_corr"),
                  title = 'The Correlation Matrix' )
            ),
            
            ## ----- ROW 2 : PLOT 
            fluidRow(
              box(width = 4,
                  plotlyOutput(outputId = "plot4_analysis"),
                  title = 'Streaming vs Tempo'),
              box(width = 4,
                  plotlyOutput(outputId = "plot6_analysis"),
                  title = 'Streaming vs Danceability'),
              box(width = 4,
                  plotlyOutput(outputId = "plot7_analysis"),
                  title = 'Streaming vs Speechiness')
            )
            

            
    ),
    
    # ----- PAGE 4 -----
    tabItem(tabName = "page4",
            fluidPage(
              box(width = 12,
                  title = "This dashboard created by Christopher Nindyo. You can contact me below.",
                  selectInput('website', 'Choose a website'
                              , list(linkedIn = "https://www.linkedin.com/in/christopher-nindyo-b31681188"
                                     , github = "https://github.com/ChristoNindyo"
                                     , twitter = "https://twitter.com/ChristoNindyo")
                  )
                  , htmlOutput("mySite")
              )
                  ))
            ))
)


