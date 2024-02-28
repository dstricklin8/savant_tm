library(tidyverse)
library(sp)
library(sf)

# Setup Functions ----

mutate_data <- function(input) {
  
  tm_df <- input
  
  tm_df <- tm_df %>% 
    filter(!is.na(RelSpeed)) %>% 
    mutate(
      PlateLocSide = 12*PlateLocSide,
      PlateLocHeight = 12*PlateLocHeight
    )
  
  tm_df <- tm_df %>% mutate(
    PlayResult = case_when(
      KorBB == "Strikeout" ~ "Strikeout",
      KorBB == "Walk" ~ "Walk", 
      PlayResult == "Undefined" ~ NA,
      TRUE ~ PlayResult),
    
    PlayResult = factor(PlayResult, levels = c("Single", "Double", "Triple", "HomeRun",
                                               "Walk", "Strikeout", "Out", "Error", "Sacrifice",
                                               "FieldersChoice")),
    
    AutoHitType = factor(AutoHitType, levels = c("GroundBall", "LineDrive", "FlyBall", "Popup")),
    
    PitchCall = factor(PitchCall, levels = c("BallCalled", "BallinDirt", "BallIntentional", "InPlay",
                                             "StrikeCalled", "FoulBall", "StrikeSwinging", "HitByPitch")),
    
    ynhit = case_when(
      PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
      PlayResult %in% c("Strikeout", "Out", "Error", "FieldersChoice") ~ 0,
      PlayResult == "Undefined" ~ NA
    ),
    
    launch_speed_angle = case_when(
      ExitSpeed * 1.5 - Angle >= 117 &
        ExitSpeed + Angle >= 124 &
        ExitSpeed >= 98 &
        Angle >= 4 & Angle <= 50 ~ "Barrel",
      
      ExitSpeed * 1.5 - Angle >= 111 &
        ExitSpeed + Angle >= 119 &
        ExitSpeed >= 95 &
        Angle >= 0 & Angle <= 52 ~ "Solid_Contact",
      
      ExitSpeed * 2 - Angle >= 87 &
        Angle <= 41 & 
        ExitSpeed * 2 + Angle <= 175 &
        ExitSpeed + Angle * 1.3 >= 89 &
        ExitSpeed >= 59 & Angle <= 72 ~ "Flare_or_Burner",
      
      ExitSpeed + Angle * 1.3 <= 112 &
        ExitSpeed + Angle * 1.55 >= 92 &
        ExitSpeed >= 72 & Angle <= 86 ~ "Flare_or_Burner",
      
      Angle <= 20 &
        ExitSpeed + Angle * 2.4 >= 98 &
        ExitSpeed >= 86 & ExitSpeed <= 95 ~ "Flare_or_Burner",
      
      ExitSpeed - Angle >= 76 &
        ExitSpeed + Angle * 2.4 >= 98 &
        ExitSpeed >= 95 &
        Angle <= 30 ~ "Flare_or_Burner",
      
      ExitSpeed + Angle * 2 >= 116 ~  "Poorly_Under",
      
      ExitSpeed + Angle * 2 <= 116 ~  "Poorly_Topped",
      
      ExitSpeed <= 59 ~ "Poorly_Weak"
    ),
    
    launch_speed_angle = factor(launch_speed_angle, levels = c(
      "Barrel", "Solid_Contact", "Flare_or_Burner", "Poorly_Under", "Poorly_Topped", "Poorly_Weak"
    ))
  )
  
  tm_df <- tm_df %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
    mutate(
      Date = format(Date, "%b-%d")
    )
  
  tm_df <- tm_df %>% 
    mutate(
      swing = ifelse(
        PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 1, 0),
      whiff = ifelse(
        PitchCall %in% c("StrikeSwinging"), 1, 0)
    )
  
  return(tm_df)
}
zone_poly_setup <- function(input, batter){
  
  require(sp)
  require(sf)
  require(tidyverse)
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      )
    )
  
  #### Data Frame ----
  df_pitches <- df %>%
    filter(Batter == batter) %>%
    group_by(pitch_zone) %>%
    summarise(
      Pitches = n(),
      EV = mean(ExitSpeed, na.rm = TRUE),
      LA = mean(Angle, na.rm = TRUE), 
      Hits = mean(ynhit, na.rm = TRUE),
      Swings = mean(swing, na.rm = TRUE),
      Whiffs = mean(whiff, na.rm = TRUE)
    ) %>%
    mutate(
      Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
      Freq_NumPitches = Pitches,
      Freq_EV = EV,
      Freq_LA = LA,
      Freq_Hits = Hits,
      Freq_Swings = Swings * 100,
      Freq_Whiffs = Whiffs * 100
    )
  
  #### Fortify Polys ----
  df_sz_polys = fortify(Strikezone_Polys, region = "id")
  
  #### Fix Column Names ----
  colnames(df_sz_polys)[1] <- "x"
  colnames(df_sz_polys)[2] <- "z"
  colnames(df_sz_polys)[6] <- "pitch_zone"
  
  #### Join Data and Polys ----
  df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
  
  return(df_pitch_polys)
}
zone_label_setup <- function(input){
  
  #### Labels ----
  labels <- input %>%
    select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV,
           Freq_LA, Freq_Hits, Freq_Swings, Freq_Whiffs) %>%
    distinct() %>%
    arrange(pitch_zone)
  
  #### Label Coordinates ----
  label_info <- data.frame(
    pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                   "KZONE_04", "KZONE_05", "KZONE_06",
                   "KZONE_07", "KZONE_08", "KZONE_09",
                   "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
    x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
    z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
  )
  
  #### Join Label Coords ----
  label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
  
  return(label_coords)
}

# Scatterplot Functions ----
#### Pitch Types
pitch_types <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
 p <- ggplot() +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    geom_point(df %>% filter(Batter == batter) %>% mutate(ump_count = paste0(Balls, "-", Strikes)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = pitch_tag,
                             text = paste("", AutoPitchType,'<br>',
                                          "Batter:", Batter,"<br>", "Pitcher:", Pitcher, "<br>",
                                          "Pitch Speed:", round(RelSpeed,digits = 1), "<br>",
                                          "Inning:", Inning, "<br>",
                                          "Count:", ump_count, "<br>", "Outs:", Outs, "<br>",
                                          "Result:", PitchCall)),
               size = 3, shape = 21, show.legend = F) +
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

#### Pitch Result
pitch_result <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter == batter, !is.na(PlayResult)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
               size = 5, shape = 21, show.legend = F) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = result_colors) +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
pitch_description <- function(batter, df) {
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter == batter),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall),
               size = 5, shape = 21, show.legend = F) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
    scale_fill_manual(values = call_colors) +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
batted_ball_type <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter == batter, !is.na(AutoHitType)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType),
               size = 5, shape = 21, show.legend = F) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    scale_fill_manual(values = hit_colors) +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
contact_type <- function(batter, df) {
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  load(file = "data/aesthetics.rda")
  
  ggplot() +
    geom_point(df %>% filter(Batter == batter, !is.na(launch_speed_angle)),
               mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = launch_speed_angle),
               size = 5, shape = 21, show.legend = F) +
    geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
    geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
    geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
pitch_heatmap <- function(batter, df) {
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
swing_heatmap <- function(batter, df) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
hardhit_heatmap <- function(batter, df) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
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
#### Zone - Pitch Percent
zone_pitch_perc <- function(batter, df_input, label_input) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_PitchPercent) + 5),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
              size = 5, fontface = "bold", family = "mono") +
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
zone_total_pitches <- function(batter, df_input, label_input) {
  
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
    geom_text(label_input, 
              mapping = aes(x, z, label = Freq_NumPitches),
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

#### Zone - Exit Velocity
zone_exit_velo <- function(batter, df_input, label_input) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdBu",
                         limits = c(25, max(df_input$Freq_EV)),
                         na.value = "lightgrey", guide = "legend") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
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

#### Zone - Launch Angle
zone_launch_angle <- function(batter, df_input, label_input) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_gradientn(colours = c("#2166ac",'white', '#b2182b', "#b2182b", '#b2182b', 'white', "#2166ac"),
                         limits = c(0, 45), na.value = "#2166ac") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_LA, digits = 1))),
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

#### Zone - Hit Percentage
zone_hit_perc <- function(batter, df_input, label_input) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Hits)),
                         na.value = "lightgrey") +
    geom_text(label_input, 
              mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Hits, digits = 1))),
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
zone_swing_perc <- function(batter, df_input, label_input) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Swings)),
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
zone_whiff_perc <- function(batter, df_input, label_input) {
  
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
    coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
    scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Whiffs)),
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
save(mutate_data, zone_poly_setup, zone_label_setup, file = "data/functions.rda")







