
# TrackMan Illustrator ----------------------------------------------------

library(tidyverse)
library(shiny)
library(patchwork)
library(reactable)
library(grid)
library(ggridges)
library(sf)
library(sp)
library(rsconnect)
library(scales)
library(ggpubr)
library(bslib)
library(shinythemes)
library(htmltools)
library(cowplot)
library(ggforce)
library(gt)
library(gtExtras)
library(plotly)

load(file = "data/Strikezone_Polys.rda")
load(file = "data/functions.rda")
load(file = "data/aesthetics.rda")
load("data/app_setup.rda")
load("data/zone_setup.rda")
load("data/plot_hitters.rda")
load("data/plotly_hitters.rda")

link_shiny <- tags$a(shiny::icon("github"), "GitHub", href = "https://github.com/dstricklin8", target = "_blank")
link_posit <- tags$a(shiny::icon("twitter"), "Twitter", href = "https://twitter.com/Dstricklin8", target = "_blank")

ui <- navbarPage(
  title = "DS8 Analytics",
  bg = "#582c83", inverse = T, underline = TRUE, fillable = TRUE,
  theme = bs_theme(version = 5, bootswatch = "pulse"),
  
  tabPanel("Hitters",
           tabsetPanel(
             tabPanel("Illustrator",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          fileInput("upload_1", "Upload TrackMan .csv File", accept = c(".csv"), multiple = TRUE),
                          textInput("hitter_choice", "Enter Player Name", 
                                    value = "", placeholder = "Last, First"),
                          selectInput("chart_choice", "Select Chart",
                                      choices = chart_types, selected = NULL),
                          selectInput("pitcher_throws", "Pitcher Throws:",
                                      choices = c("All", "Right", "Left")),
                          selectInput("pitch_tag", "Pitch Tag Type:",
                                      choices = c("TaggedPitchType", "AutoPitchType"),
                                      selected = "TaggedPitchType"),
                          actionButton("goButton_1", "Update Page")),
                        
                        mainPanel(
                          layout_column_wrap(
                            width = NULL, height = 600, fill = FALSE,
                            style = css(grid_template_columns = "4fr 1fr"),
                            plotOutput("hitter_plots"),
                            plotOutput("legend_plot")
                          )
                        )
                      )
             ),
             tabPanel("Interactive",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          fileInput("upload_2", "Upload TrackMan .csv File", accept = c(".csv"), multiple = TRUE),
                          textInput("hitter_choice2", "Enter Player Name", 
                                    value = "", placeholder = "Last, First"),
                          selectInput("chart_choice2", "Select Chart",
                                      choices = c("Pitch Types", 
                                                  "Pitch Description",
                                                  "Pitch Result", 
                                                  "Batted Ball Type",
                                                  "Contact Type"), 
                                      selected = NULL),
                          selectInput("pitcher_throws2", "Pitcher Throws:",
                                      choices = c("All", "Right", "Left")),
                          selectInput("pitch_tag2", "Pitch Tag Type:",
                                      choices = c("TaggedPitchType", "AutoPitchType"),
                                      selected = "TaggedPitchType"),
                          actionButton("goButton_2", "Update Page")),
                        mainPanel(
                          layout_column_wrap(
                            width = NULL, height = 720, fill = FALSE,
                            style = css(grid_template_columns = "3fr 1fr"),
                            plotlyOutput("hitter_plotly"),
                            plotOutput("legend_plotly"))
                        )
                      )
             )
           )
  ),
  nav_spacer(), # spacer ----
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 10 * 1024^2)
  
  # Hitters ----
  df_1 <- eventReactive(input$upload_1, {
    combined <- lapply(input$upload_1$datapath, read_csv)
    do.call(rbind, combined)
  })
  df_2 <- eventReactive(input$upload_2, {
    combined <- lapply(input$upload_2$datapath, read_csv)
    do.call(rbind, combined)
  })
  
  data_1 <- eventReactive(input$goButton_1, {
    # Load Files 
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    
    #### Mutate data_1 
    tm_df <- mutate_data(df_1())
    
    # Pitcher Handness 
    if (input$pitcher_throws == "Right") {tm_df <- tm_df %>% filter(PitcherThrows == "Right")}
    else if (input$pitcher_throws == "Left") {tm_df <- tm_df %>% filter(PitcherThrows == "Left")}
    
    tm_df <- tm_df %>% 
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
          TRUE ~ TaggedPitchType
        ))
    
    # Tag Type
    if (input$pitch_tag == "TaggedPitchType") {tm_df$pitch_tag <- tm_df$TaggedPitchType}
    else if (input$pitch_tag == "AutoPitchType") {tm_df$pitch_tag <- tm_df$AutoPitchType}
    
    tm_df <- tm_df %>% 
      mutate(pitch_tag = fct_infreq(pitch_tag))
    
    return(tm_df)})
  data_2 <- eventReactive(input$goButton_2, {
    # Load Files 
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    
    #### Mutate data_1 
    tm_df <- mutate_data(df_2())
    
    # Pitcher Handness 
    if (input$pitcher_throws2 == "Right") {tm_df <- tm_df %>% filter(PitcherThrows == "Right")}
    else if (input$pitcher_throws2 == "Left") {tm_df <- tm_df %>% filter(PitcherThrows == "Left")}
    
    tm_df <- tm_df %>% 
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
          TRUE ~ TaggedPitchType
        ))
    
    # Tag Type
    if (input$pitch_tag2 == "TaggedPitchType") {tm_df$pitch_tag <- tm_df$TaggedPitchType}
    else if (input$pitch_tag2 == "AutoPitchType") {tm_df$pitch_tag <- tm_df$AutoPitchType}
    
    tm_df <- tm_df %>% 
      mutate(pitch_tag = fct_infreq(pitch_tag))
    
    return(tm_df)})
  
  poly_df_1 <- eventReactive(input$goButton_1, {
    # Load Files 
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    tm_poly <- zone_poly_setup(data_1(), input$hitter_choice)
    return(tm_poly)
  })
  poly_df_2 <- eventReactive(input$goButton_2, {
    # Load Files 
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    tm_poly <- zone_poly_setup(data_2(), input$hitter_choice2)
    return(tm_poly)
  })
  
  label_df_1 <- eventReactive(input$goButton_1, {
    # Load Files 
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    tm_label <- zone_label_setup(poly_df_1())
    return(tm_label)
  })
  label_df_2 <- eventReactive(input$goButton_2, {
    # Load Files 
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    tm_label <- zone_label_setup(poly_df_2())
    return(tm_label)
  })
  
  hitterPlot <- eventReactive(input$goButton_1, {
    if (input$chart_choice == "Zone - Total Pitches") {
      plot_zone_total_pitches(input$hitter_choice, poly_df_1(), label_df_1())
    }
    else if (input$chart_choice == "Zone - Pitch Percentage") {
      plot_zone_pitch_perc(input$hitter_choice, poly_df_1(), label_df_1())
    }
    else if (input$chart_choice == "Zone - Exit Velocity") {
      plot_zone_exit_velo(input$hitter_choice, poly_df_1(), label_df_1())
    }
    else if (input$chart_choice == "Zone - Launch Angle") {
      plot_zone_launch_angle(input$hitter_choice, poly_df_1(), label_df_1())
    }
    else if (input$chart_choice == "Zone - Batting Avg.") {
      plot_zone_hit_perc(input$hitter_choice, poly_df_1(), label_df_1())
    }
    else if (input$chart_choice == "Zone - Swing Percentage") {
      plot_zone_swing_perc(input$hitter_choice, poly_df_1(), label_df_1())
    }
    else if (input$chart_choice == "Zone - Whiff Percentage") {
      plot_zone_whiff_perc(input$hitter_choice, poly_df_1(), label_df_1())
    }
    # Scatter Plots ----
    else if (input$chart_choice == "Pitch Types") {
      plot_pitch_types(input$hitter_choice, data_1())
    }
    else if (input$chart_choice == "Pitch Description") {
      plot_pitch_description(input$hitter_choice, data_1())
    }
    else if (input$chart_choice == "Pitch Result") {
      plot_pitch_result(input$hitter_choice, data_1())
    }
    else if (input$chart_choice == "Batted Ball Type") {
      plot_batted_ball_type(input$hitter_choice, data_1())
    }
    else if (input$chart_choice == "Contact Type") {
      plot_contact_type(input$hitter_choice, data_1())
    }
    # Heatmaps ----
    else if (input$chart_choice == "Pitch Heatmap") {
      plot_pitch_heatmap(input$hitter_choice, data_1())
    }
    else if (input$chart_choice == "Swing Heatmap") {
      plot_swing_heatmap(input$hitter_choice, data_1())
    }
    else if (input$chart_choice == "Hard-Hit Heatmap") {
      plot_hardhit_heatmap(input$hitter_choice, data_1())
    }
    
  })
  
  hitterPlotly <- eventReactive(input$goButton_2, {
    if (input$chart_choice2 == "Pitch Types") {
      plotly_pitch_types(input$hitter_choice2, data_2())
    }
    else if (input$chart_choice2 == "Pitch Description") {
      plotly_pitch_description(input$hitter_choice2, data_2())
    }
    else if (input$chart_choice2 == "Pitch Result") {
      plotly_pitch_result(input$hitter_choice2, data_2())
    }
    else if (input$chart_choice2 == "Batted Ball Type") {
      plotly_batted_ball_type(input$hitter_choice2, data_2())
    }
    else if (input$chart_choice2 == "Contact Type") {
      plotly_contact_type(input$hitter_choice2, data_2())
    }
    
  })
  
  legends_1 <- eventReactive(input$goButton_1, {
    # Load Files
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    
    # Legend Options ----
    type_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = pitch_tag), shape = 21, size = 6) +
      scale_fill_manual(values = pitch_colors)
    
    result_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = PlayResult), shape = 21, size = 6) +
      scale_fill_manual(values = result_colors)
    
    call_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = PitchCall), shape = 21, size = 6) +
      scale_fill_manual(values = call_colors)
    
    hit_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = AutoHitType), shape = 21, size = 6) +
      scale_fill_manual(values = hit_colors)
    
    contact_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice, !is.na(launch_speed_angle))) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = launch_speed_angle), shape = 21, size = 6)
    
    # Plot Legends ----
    plot_legend <- function(legend_choice) {
      
      legend <- get_legend(
        legend_choice +
          theme_minimal() +
          guides(colour = guide_legend(ncol = 1)) +
          theme(
            legend.key.size = unit(rel(1.2), 'cm'),
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_blank(),
            legend.box.background = element_blank(),
            legend.background = element_blank(),
            legend.box = element_blank(),
            legend.box.margin = margin(0, -1, 0, -1),
            legend.position = "right",
            legend.box.just = "center",
            legend.direction = "vertical"
          ))
      
      plot_grid(legend, ncol = 1)
    }
    
    if (input$chart_choice == "Pitch Types") {plot_legend(type_legend)}
    else if (input$chart_choice == "Pitch Description") {plot_legend(call_legend)}
    else if (input$chart_choice == "Pitch Result") {plot_legend(result_legend)}
    else if (input$chart_choice == "Batted Ball Type") {plot_legend(hit_legend)}
    else if (input$chart_choice == "Contact Type") {plot_legend(contact_legend)}
  })
  legends_2 <- eventReactive(input$goButton_2, {
    # Load Files
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/functions.rda")
    load(file = "data/aesthetics.rda")
    
    # Legend Options ----
    type_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = pitch_tag), shape = 21, size = 6) +
      scale_fill_manual(values = pitch_colors)
    
    result_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = PlayResult), shape = 21, size = 6) +
      scale_fill_manual(values = result_colors)
    
    call_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = PitchCall), shape = 21, size = 6) +
      scale_fill_manual(values = call_colors)
    
    hit_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = AutoHitType), shape = 21, size = 6) +
      scale_fill_manual(values = hit_colors)
    
    contact_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2, !is.na(launch_speed_angle))) +
      geom_point(aes(PlateLocSide, PlateLocHeight, fill = launch_speed_angle), shape = 21, size = 6)
    
    # Plot Legends ----
    plot_legend <- function(legend_choice) {
      
      legend <- get_legend(
        legend_choice +
          theme_minimal() +
          guides(colour = guide_legend(ncol = 1)) +
          theme(
            legend.key.size = unit(rel(1.2), 'cm'),
            legend.text = element_text(size = rel(1.2)),
            legend.title = element_blank(),
            legend.box.background = element_blank(),
            legend.background = element_blank(),
            legend.box = element_blank(),
            legend.box.margin = margin(0, -1, 0, -1),
            legend.position = "right",
            legend.box.just = "center",
            legend.direction = "vertical"
          ))
      
      plot_grid(legend, ncol = 1)
    }
    
    if (input$chart_choice2 == "Pitch Types") {plot_legend(type_legend)}
    else if (input$chart_choice2 == "Pitch Description") {plot_legend(call_legend)}
    else if (input$chart_choice2 == "Pitch Result") {plot_legend(result_legend)}
    else if (input$chart_choice2 == "Batted Ball Type") {plot_legend(hit_legend)}
    else if (input$chart_choice2 == "Contact Type") {plot_legend(contact_legend)}
  })
  
  output$hitter_plots <- renderPlot({hitterPlot()})
  output$legend_plot <- renderPlot({legends_1()})
  
  output$hitter_plotly <- renderPlotly({hitterPlotly()})
  output$legend_plotly <- renderPlot({legends_2()})
}


shinyApp(ui, server)

