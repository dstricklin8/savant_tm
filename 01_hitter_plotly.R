library(tidyverse)
library(sp)
library(sf)
library(plotly)

# Scatter plot Functions ----
# Pitch Types
plotly_pitch_types <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  p <- ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    geom_point(df %>% filter(Batter == batter) %>% mutate(ump_count = paste0(Balls, "-", Strikes)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = pitch_tag,
                             text = paste("", pitch_tag,'<br>',
                                          "Batter:", Batter, "<br>",
                                          "Pitcher:", Pitcher, "<br>",
                                          "Pitcher Throws:", PitcherThrows, "<br>",
                                          "Pitch Speed:", round(RelSpeed, digits = 1), "<br>",
                                          "Exit Velo:", ifelse(!is.na(ExitSpeed), round(ExitSpeed, digits = 1), "--"), "<br>",
                                          "Launch Angle:", ifelse(!is.na(Angle), round(Angle, digits = 1), "--"), "<br>",
                                          "Inning:", Inning, "<br>",
                                          "Count:", ump_count,"", "Outs:", Outs, "<br>",
                                          "Result:", PitchCall)),
               size = 3, shape = 21, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_manual(values = pitch_colors) +
    theme_minimal() +
    labs(fill = "") +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    )
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(hoverlabel=list(bgcolor="white")) %>% 
    hide_legend()
  
}

# Pitch Desc.
plotly_pitch_description <- function(batter, df) {
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  p <- ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter) %>% mutate(ump_count = paste0(Balls, "-", Strikes)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall,
                             text = paste("", pitch_tag,'<br>',
                                          "Batter:", Batter,"<br>",
                                          "Pitcher:", Pitcher, "<br>",
                                          "Pitcher Throws:", PitcherThrows, "<br>",
                                          "Pitch Speed:", round(RelSpeed, digits = 1), "<br>",
                                          "Exit Velo:", ifelse(!is.na(ExitSpeed), round(ExitSpeed, digits = 1), "--"), "<br>",
                                          "Launch Angle:", ifelse(!is.na(Angle), round(Angle, digits = 1), "--"), "<br>",
                                          "Inning:", Inning, "<br>",
                                          "Count:", ump_count, "", "Outs:", Outs, "<br>",
                                          "Result:", PitchCall)),
               size = 3, shape = 21, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
    scale_fill_manual(values = call_colors) +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    )
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(hoverlabel=list(bgcolor="white")) %>% 
    hide_legend()
}

# Pitch Result
plotly_pitch_result <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  p <- ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter & !is.na(PlayResult)) %>% mutate(ump_count = paste0(Balls, "-", Strikes)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult,
                             text = paste("", pitch_tag,'<br>',
                                          "Batter:", Batter,"<br>",
                                          "Pitcher:", Pitcher, "<br>",
                                          "Pitcher Throws:", PitcherThrows, "<br>",
                                          "Pitch Speed:", round(RelSpeed, digits = 1), "<br>",
                                          "Exit Velo:", ifelse(!is.na(ExitSpeed), round(ExitSpeed, digits = 1), "--"), "<br>",
                                          "Launch Angle:", ifelse(!is.na(Angle), round(Angle, digits = 1), "--"), "<br>",
                                          "Inning:", Inning, "<br>",
                                          "Count:", ump_count,"", "Outs:", Outs, "<br>",
                                          "Result:", PitchCall)),
               size = 3, shape = 16, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = result_colors) +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    )
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(hoverlabel=list(bgcolor="white")) %>% 
    hide_legend()
}

# Batted Ball Type
plotly_batted_ball_type <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  p <- ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter, !is.na(AutoHitType)) %>%
                 mutate(ump_count = paste0(Balls, "-", Strikes)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType,
                             text = paste("", pitch_tag,'<br>',
                                          "Batter:", Batter,"<br>",
                                          "Pitcher:", Pitcher, "<br>",
                                          "Pitcher Throws:", PitcherThrows, "<br>",
                                          "Pitch Speed:", round(RelSpeed, digits = 1), "<br>",
                                          "Exit Velo:", ifelse(!is.na(ExitSpeed), round(ExitSpeed, digits = 1), "--"), "<br>",
                                          "Launch Angle:", ifelse(!is.na(Angle), round(Angle, digits = 1), "--"), "<br>",
                                          "Inning:", Inning, "<br>",
                                          "Count:", ump_count, "", "Outs:", Outs, "<br>",
                                          "Result:", PitchCall)),
               size = 3, shape = 21, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = hit_colors) +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    labs(
      fill = ""
    )+
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    )
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(hoverlabel=list(bgcolor="white")) %>% 
    hide_legend()
}

# Contact Type
plotly_contact_type <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  p <- ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter, !is.na(launch_speed_angle)) %>%
                 mutate(ump_count = paste0(Balls, "-", Strikes)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = launch_speed_angle,
                             text = paste("", pitch_tag,'<br>',
                                          "Batter:", Batter,"<br>",
                                          "Pitcher:", Pitcher, "<br>",
                                          "Pitcher Throws:", PitcherThrows, "<br>",
                                          "Pitch Speed:", round(RelSpeed, digits = 1), "<br>",
                                          "Exit Velo:", ifelse(!is.na(ExitSpeed), round(ExitSpeed, digits = 1), "--"), "<br>",
                                          "Launch Angle:", ifelse(!is.na(Angle), round(Angle, digits = 1), "--"), "<br>",
                                          "Inning:", Inning, "<br>",
                                          "Count:", ump_count, "", "Outs:", Outs, "<br>",
                                          "Result:", PitchCall)),
               size = 3, shape = 21, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    )
  
  ggplotly(p, tooltip = c("text")) %>% 
    layout(hoverlabel=list(bgcolor="white")) %>% 
    hide_legend()
}


# Save Data ----
save(plotly_pitch_types, plotly_pitch_description, plotly_pitch_result,
     plotly_batted_ball_type, plotly_contact_type, file = "data/plotly_hitters.rda")
