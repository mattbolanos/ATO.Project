library(tidyverse)
library(extrafont)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(htmltools)

#Source data from main.R
source("main.R")


#User Interface
ui <- navbarPage(theme = shinytheme("lumen"), 
  "NBA After Time Outs (ATOs) PPP Rates and Postseason Success",
  tabPanel("Welcome",
           strong("NBA ATOs PPP Rates and Playoff Success",
                  style = "font-size:35px; font-family:Georgia; color:black"),
           br(),
           strong("Created by Matt Bolaños | matthew.a.bolanos@gmail.com | matthewabolanos.com",
                  style = "font-size:16px;font-family:Georgia; color:black"),
           br(),
           br(),
           p("This app visualizes NBA After Time Out (ATOs) data from the 2009-10
           to the 2018-19 seasons. ATOs are one way to measure how adept coaches
           are at recognizing and exploiting opponent weakpoints mid-game, by drawing
           up quick hitters such as quick slips, hammer actions, etc. My goal of this
           project was to explore if teams with high PPP rates on ATOs in the regular
           season maintained those rates in the postseason, and if there was any
           association between regular season ATO PPP and playoff success.", 
             style= "font-size:16px; font-family:Arial;color:black"),
           br(),
           strong("Method and Takeaways",
                  style = "font-size:20px;font-family:Georgia; color:black"),
           br(),
           br(),
           p("I gathered ATO data from Synergy and playoff win data from nbastatR.
             I calculated the league average ATO PPP rate for each indivudal regular season
             and postseaso. Since the variance of ATO PPP is relatively small,
             teams were categorized as above average or below average to give a 
             better idea of how each team performed relative to the rest of the league
             in that stretch. Since playoff PPP and wins were comparison metrics,
             only teams that made the playoffs in a given year are included on the 
             plots There are some interesting takeways from the visualizations.
             One being that every team that has reached the NBA Finals in the 
             past decade was above average in regular season ATO PPP in the corresponding 
             regular season. This suggets that being able to efficiently convert ATO
             opportunities bodes well for teams in the playoffs, as opponents really
             dig into team's playbooks and sets, forcing on-the-fly adjustments. There
             was not a strong relationship between regular season and postseason ATO PPP,
             however. This may have been a result of differently sized samples, since
             some teams only play 4 playoff games compared to 82 regular season ones.
             These first round exit teams may have skewed the data, since some 
             were hyperefficient on relatively fewer possessions than teams that made
             deep playoff runs (e.g. the 2014-15 Charlotte Hornets).",
             style= "font-size:16px; font-family:Arial;color:black"),
           br(),
           p("I would like to shoutout Owen Phillips over at the https://thef5.substack.com
             for making incredible NBA data visualizations, thorough R tutorials,
             and quickly making Consolas my go-to font for charts.",
             style= "font-size:16px; font-family:Arial;color:black")),
  #Second tab
  tabPanel("Regular Season vs Postseason ATO PPP",
            strong("Created by Matt Bolaños | matthew.a.bolanos@gmail.com",style = "font-size:18px;font-family:Georgia; color:black"),
            br(),
            br(),
            sidebarLayout(sidebarPanel(strong("Filter for Team(s) and/or Season(s).",style = "color:black"),
                br(),
                br(),
                strong("At least one option in each must be selected.",style = "color:black"),
                br(),
                br(),
                strong("If the default values are selected, all of the data for that variable will 
                       show on plot. Deselect them for proper filtering", style = "color:black"),
                br(),
                br(),
                pickerInput('year1', 'Season(s)', choices = c("2009-2019", unique(full_df$year)),
                            selected = "2009-2019", width = '100%', multiple = TRUE),
                pickerInput('team1', 'Team(s)', choices = c("All Teams", unique(full_df$Team)),
                            selected = 'All Teams', width = '100%', multiple = TRUE),
              width = 3),
              mainPanel(plotOutput("plot1",hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce", nullOutside = FALSE)),
                        uiOutput("hover_info"), width = 9))),
  #Third tab
  tabPanel("Regular Season ATO PPP vs Postseason Wins",
           strong("Created by Matt Bolaños | matthew.a.bolanos@gmail.com",
                  style = "font-size:18px;font-family:Georgia; color:black"),
           br(),
           br(),
           sidebarLayout(sidebarPanel(strong("Filter for Team(s) and/or Season(s).",style = "color:black"),
               br(),
               br(),
               strong("At least one option in each must be selected.",style = "color:black"),
               br(),
               br(),
               strong("If the default values are selected, all of the data for that variable will 
                       show on plot. Deselect them for proper filtering", style = "color:black"),
               br(),
               br(),
               pickerInput('year2', 'Season(s)', choices = c("2009-2019", unique(full_df$year)),
                           selected = "2009-2019", width = '100%', multiple = TRUE),
               pickerInput('team2', 'Team(s)', choices = c("All Teams", unique(full_df$Team)),
                           selected = 'All Teams', width = '100%', multiple = TRUE),
               width = 3),
             mainPanel(plotOutput("plot2",hover = hoverOpts("plot_hover2", delay = 100, delayType = "debounce", nullOutside = FALSE)),
                       uiOutput("hover_info2"), width = 9))))

#Server
server <- function(input, output) {
  #Load theme and data
  full_df <- full_df
  my_theme <- function () {
    theme_minimal(base_size=15, base_family="Consolas") %+replace%
      theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#fff8f0", color = "#fff8f0"),
        axis.title = element_text(face='bold')
      )
  }
  
  
  #First tab plot
  output$plot1 <- renderPlot({
 
    vals = reactiveValues(
      year_sel = "",
      team_sel = ""
    )
    
    vals$year_sel <- if (((input$year1)== "2009-2019") | nchar(input$year1) == 0) unique(as.vector(full_df$year)) else as.vector(input$year1)
    vals$team_sel <- if (((input$team1)== "All Teams") | nchar(input$team1) == 0) unique(as.vector(full_df$Team)) else as.vector(input$team1)
    final_df <- filter(full_df, year %in% vals$year_sel, Team %in% vals$team_sel)
    
    ggplot(final_df, aes(x = reg_season_PPP_comp, y = playoffs_PPP_comp, fill = primary))+
      geom_point(size=6.5,aes(group = Team, color = primary))+
      scale_color_identity()+
      scale_x_continuous(limits = c(-.125, .155))+
      scale_y_continuous(limits = c(-.28,.275))+
      geom_vline(xintercept = 0, linetype = 1) +
      geom_hline(yintercept = 0, linetype = 1) +
      geom_abline(linetype =2)+
      geom_text(size=5,aes(.075, -.275,
                           label = "Above League Average"), family='Consolas')+
      geom_text(size=5,aes(-.0575, -.275,
                           label = "Below League Average"), family='Consolas')+
      geom_text(size=5,aes(-.115, .13,
                           label = "Above League Average"), angle = 90,family='Consolas')+
      geom_text(size=5,aes(-.115,-.14,
                           label = "Below League Average"), angle = 90,family='Consolas')+
      my_theme()+
      labs(title = paste0("NBA ", paste0(input$year1, collapse = ", "), " Regular Season ATO PPP vs Playoff ATO PPP For ",
                          paste0(input$team1, collapse = ", ")),
           x = paste0("Regular Season ATO PPP"),
           y = paste0("Playoff ATO PPP"),
           caption="Data: Synergy | Chart: Matt Bolaños")+
      theme(legend.position = "none",
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.text = element_text(size=10),
            plot.title = element_text(size=15))
    
  },height = 575)
  
  
  #Hover panel for first plot
  output$hover_info <- renderUI({
    vals = reactiveValues(
      year_sel = "",
      team_sel = ""
    )
    vals$year_sel <- if (((input$year1)== "2009-2019") | nchar(input$year1) == 0) unique(as.vector(full_df$year)) else as.vector(input$year1)
    vals$team_sel <- if (((input$team1)== "All Teams") | nchar(input$team1) == 0) unique(as.vector(full_df$Team)) else as.vector(input$team1)
    final_df <- filter(full_df, year %in% vals$year_sel, Team %in% vals$team_sel)

    
    hover <- input$plot_hover
    point <- nearPoints(final_df, hover, threshold =20, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)
    
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px, "px; top:", top_px, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Team: </b>", point$Team, "<br/>",
                    "<b> Season: </b>", point$year, "<br/>",
                    "<b> Reg Season PPP: </b>", point$reg_season_PPP, "<br/>",
                    "<b> Playoffs PPP: </b>", point$playoffs_PPP, "<br/>"))))
  })
  
  #Second tab plot
  output$plot2 <- renderPlot({
    
    vals = reactiveValues(
      year_sel = "",
      team_sel = ""
    )
    
    vals$year_sel <- if (((input$year2)== "2009-2019") | nchar(input$year2) == 0) unique(as.vector(full_df$year)) else as.vector(input$year2)
    vals$team_sel <- if (((input$team2)== "All Teams") | nchar(input$team2) == 0) unique(as.vector(full_df$Team)) else as.vector(input$team2)

    final_df <- filter(full_df, year %in% vals$year_sel, Team %in% vals$team_sel)
    
    ggplot(final_df, aes(x = reg_season_PPP_comp, y = playoff_wins, fill = primary))+
      geom_point(size=6, aes(group = Team, color = primary))+
      scale_color_identity()+
      scale_x_continuous(limits = c(-.125, .155))+
      scale_y_continuous(limits = c(-1.5,17))+
      geom_vline(xintercept = 0, linetype = 1) +
      my_theme()+
      theme(legend.position = "none",
            axis.text.x=element_blank(),
            axis.text = element_text(size=10),
            plot.title = element_text(size=15))+
      labs(title = paste0("NBA ", paste0(input$year2, collapse = ", "), " Regular Season ATO PPP vs Playoff Wins For ",
                          paste0(input$team2, collapse = ", ")),
           x = paste0("Regular Season ATO PPP"),
           y = paste0("Playoff Wins"),
           caption = "Data: Synergy | Chart: Matt Bolaños")+
      geom_text(size=5,aes(.075,-1,label = "Above League Average"), family='Consolas')+
      geom_text(size=5,aes(-.05,-1,label = "Below League Average"), family='Consolas')
    
  },height=575)
  
  #Hover panel for second plot
  output$hover_info2 <- renderUI({
    vals = reactiveValues(
      year_sel = "",
      team_sel = ""
    )
    
    vals$year_sel <- if (((input$year2)== "2009-2019") | nchar(input$year2) == 0) unique(as.vector(full_df$year)) else as.vector(input$year2)
    vals$team_sel <- if (((input$team2)== "All Teams") | nchar(input$team2) == 0) unique(as.vector(full_df$Team)) else as.vector(input$team2)
    
    final_df <- filter(full_df, year %in% vals$year_sel, Team %in% vals$team_sel)
    
    
    hover <- input$plot_hover2
    point <- nearPoints(final_df, hover, threshold =20, maxpoints = 1)
    if (nrow(point) == 0) return(NULL)
    
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px, "px; top:", top_px, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Team: </b>", point$Team, "<br/>",
                    "<b> Season: </b>", point$year, "<br/>",
                    "<b> Reg Season PPP: </b>", point$reg_season_PPP, "<br/>",
                    "<b> Playoff Wins: </b>", point$playoff_wins, "<br/>"))))
  })
  
  
}
  
shinyApp(ui = ui, server = server)  

