
# Aesthetics --------------------------------------------------------------

# Create Pitch Colors Palette for ggplot fill FourSeamFastBall
pitch_colors <- c(
  "Sinker" = "#fe9d00",
  "Slider" = "#efe717",
  "ChangeUp" = "#1fbe3a",
  "Changeup" = "#1fbe3a",
  "Four-Seam" = "#d22d4a",
  "FourSeamFastBall" = "#d22d4a",
  "Splitter" = "#3bacac",
  "Curveball" = "#04d1ed",
  "Cutter" = "#933f1f",
  "Fastball" = "#d22d4a",
  "TwoSeamFastBall" = "maroon3",
  "Undefined" = "darkgrey",
  "Other" = "darkgrey"
)

# pitch_colors_tibble <- tibble(
#   pitch_tag = c("Sinker", "Slider", "Changeup", "ChangeUp",
#                 "Four-Seam", "Splitter", "Curveball",
#                 "Cutter", "Fastball", "TwoSeamFastBall", "Undefined", "Other"),
#   
#   pitch_hex = c("#fe9d00", "#efe717", "#1fbe3a", "#1fbe3a",
#                 "#d22d4a", "#3bacac", "#04d1ed",
#                 "#933f1f", "#d22d4a", "maroon3", "darkgrey", "darkgrey")
# )

# Pitch Call Colors ----
# unique(season$PitchCall)

call_colors <- c("BallCalled" = "#1e76b4",
                     "BallIntentional" = "#1e76b4",
                     "BallinDirt" = "#1e76b4",
                     "StrikeCalled" = "#ff7f0f",
                     "StrikeSwinging" = "#e377c3",
                     "FoulBall" = "#2ba02b",
                     "HitByPitch" = "#272727",
                     "InPlay" = "#9367bd") 

# Play Result Colors ----
# unique(season$PlayResult)

result_colors <- c("Single" = "#fe6100",
                      "Double" = "#785ef1",
                      "Triple" = "#ffb000",
                      "HomeRun" = "#dc2680",
                      "Walk" = "#565e97",
                      "Strikeout" = "#791735",
                      "Out" = "#c2c2c2",
                      "Error" = "#f8f8f8",
                      "Sacrifice" = "#f8f8f8",
                      "FieldersChoice" = "#f8f8f8"
)

# Hit Type Colors ----
# unique(season$AutoHitType)

hit_colors <- c("GroundBall" = "#b71384",
                   "FlyBall" = "#505f39",
                   "LineDrive" = "#ebed4d",
                   "Popup" = "#8eaed8"
)


save(pitch_colors, call_colors, result_colors, hit_colors, file = "data/aesthetics.rda")