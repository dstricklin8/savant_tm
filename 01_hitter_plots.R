library(tidyverse)
library(sp)
library(sf)

# Scatterplot Functions ----
#### Pitch Types
plot_pitch_types <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    geom_point(df %>% filter(Batter == batter),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = pitch_tag),
               size = 5, shape = 21, show.legend = F) +
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
  
}

#### Pitch Result
plot_pitch_result <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter, !is.na(PlayResult)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
               size = 5, shape = 21, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = result_colors) +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
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
}

#### Pitch Desc.
plot_pitch_description <- function(batter, df) {
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall),
               size = 5, shape = 21, show.legend = F) +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
    scale_fill_manual(values = call_colors) +
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
}

#### Batted Ball Type
plot_batted_ball_type <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter, !is.na(AutoHitType)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType),
               size = 5, shape = 21, show.legend = F) +
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
}

#### Contact Type
plot_contact_type <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
    geom_point(df %>% filter(Batter == batter, !is.na(launch_speed_angle)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = launch_speed_angle),
               size = 5, shape = 21, show.legend = F) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    # geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
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
}

# Heatmaps ----
#### Pitch Heatmap
plot_pitch_heatmap <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot(df %>% filter(Batter == batter),
         mapping = aes(PlateLocSide, PlateLocHeight)) +
    stat_density_2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE,
      show.legend = FALSE
    ) +
    scale_fill_gradient(low = "white", high = "red") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(kzone_11, mapping = aes(x, z), color = "black") +
    geom_path(kzone_12, mapping = aes(x, z), color = "black") +
    geom_path(kzone_13, mapping = aes(x, z), color = "black") +
    geom_path(kzone_14, mapping = aes(x, z), color = "black") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
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
}

#### Swing Heatmap
plot_swing_heatmap <- function(batter, df) {
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  
  df %>% 
    mutate(swing = if_else(swing == 1, TRUE, FALSE)) %>%
    filter(Batter == batter, swing) %>% 
    ggplot(aes(PlateLocSide, PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)),
                    geom = "raster", contour = FALSE, show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "red") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(kzone_11, mapping = aes(x, z), color = "black") +
    geom_path(kzone_12, mapping = aes(x, z), color = "black") +
    geom_path(kzone_13, mapping = aes(x, z), color = "black") +
    geom_path(kzone_14, mapping = aes(x, z), color = "black") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
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
}

#### Hard-Hit Heatmap
plot_hardhit_heatmap <- function(batter, df) {
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  
  df %>% 
    filter(Batter == batter, ExitSpeed >= 95) %>% 
    ggplot(aes(PlateLocSide, PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)),
                    geom = "raster", contour = FALSE, show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "red") +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(kzone_11, mapping = aes(x, z), color = "black") +
    geom_path(kzone_12, mapping = aes(x, z), color = "black") +
    geom_path(kzone_13, mapping = aes(x, z), color = "black") +
    geom_path(kzone_14, mapping = aes(x, z), color = "black") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    theme_minimal() +
    labs(fill = "") +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
}

# Polys ----
#### Zone - Number of Pitches
plot_zone_total_pitches <- function(batter, df_input, label_input) {
  
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_NumPitches))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_NumPitches))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_NumPitches),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_NumPitches)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(
      label_input, 
      mapping = aes(x, z, label = Freq_NumPitches), size = 5, fontface = "bold", family="mono") +
    geom_text(
      label_input %>% filter(Freq_NumPitches == max(Freq_NumPitches) | Freq_NumPitches == min(Freq_NumPitches)), 
      mapping = aes(x, z, label = Freq_NumPitches), size = 5, fontface = "bold", family="mono", colour = "white") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Pitch Percent
plot_zone_pitch_perc <- function(batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_PitchPercent))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_PitchPercent))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_PitchPercent),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_PitchPercent)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(
      label_input, 
      mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
      size = 5, fontface = "bold", family = "mono") +
    geom_text(
      label_input %>% filter(Freq_PitchPercent == max(Freq_PitchPercent) | Freq_PitchPercent == min(Freq_PitchPercent)), 
      mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
      size = 5, fontface = "bold", family = "mono", colour = "white") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Exit Velocity
plot_zone_exit_velo <- function(batter, df_input, label_input) {
  
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_EV))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_EV))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_EV),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu",
                         limits = c(25, max(df_input$Freq_EV)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
              size = 5, fontface = "bold", family="mono") +
    geom_text(label_input %>% filter(Freq_EV == max(Freq_EV) | Freq_EV == min(Freq_EV)), 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
              size = 5, fontface = "bold", family="mono", colour = "white") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Launch Angle
plot_zone_launch_angle <- function(batter, df_input, label_input) {
  
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_LA))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_LA))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_LA),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_gradientn(colours = c("#2166ac",'white', '#b2182b', "#b2182b", '#b2182b', 'white', "#2166ac"),
                         limits = c(0, 45), na.value = "#2166ac") +
    geom_text(label_input, 
              mapping = aes(x, z, label = round(Freq_LA, digits = 0)),
              size = 5, fontface = "bold", family="mono") +
    geom_text(label_input %>% filter(Freq_LA >= 15 & Freq_LA <= 20 | Freq_LA < 0 | Freq_LA >= 45), 
              mapping = aes(x, z, label = round(Freq_LA, digits = 0)),
              size = 5, fontface = "bold", family="mono", colour = "white") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Hit Percentage
plot_zone_hit_perc <- function(batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_Hits))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_Hits))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Hits),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_Hits)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%.3f", round(Freq_Hits, digits = 3))),
              size = 5, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Swing Percentage
plot_zone_swing_perc <- function(batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_Swings))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_Swings))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Swings),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_Swings)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Swings, digits = 1))),
              size = 5, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

#### Zone - Swing Percentage
plot_zone_whiff_perc <- function(batter, df_input, label_input) {
  
  #### Load App Setup
  load(file = "data/app_setup.rda")
  
  #### Fix NA ----
  df_input <- df_input %>%
    filter(!is.na(Freq_Whiffs))
  
  label_input <- label_input %>% 
    filter(!is.na(Freq_Whiffs))
  
  #### Create Plot ----
  ggplot() +
    geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_path(sz_2, mapping = aes(x, z), color = "black") +
    geom_path(sz_3, mapping = aes(x, z), color = "black") +
    geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
    geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Whiffs),
                 color = "black", show.legend = FALSE) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_Whiffs)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Whiffs, digits = 1))),
              size = 5, fontface = "bold", family="mono") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.text = element_text(family = "sans", size = 12),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

# Save Data ----
save(plot_batted_ball_type, plot_pitch_result, plot_pitch_types, plot_pitch_description,
     plot_pitch_heatmap, plot_contact_type, plot_swing_heatmap, plot_hardhit_heatmap, 
     
     plot_zone_swing_perc, plot_zone_hit_perc, plot_zone_launch_angle, plot_zone_exit_velo,
     plot_zone_total_pitches, plot_zone_pitch_perc, plot_zone_swing_perc, plot_zone_whiff_perc,
     
     file = "data/plot_hitters.rda")
