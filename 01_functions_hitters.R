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


# Save Data ----
save(mutate_data, zone_poly_setup, zone_label_setup, file = "data/functions.rda")







