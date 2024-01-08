library(tidyverse)
library(here)
theme_set(theme_light())

games <- read_csv(here("data", "games.csv"))
plays <- read_csv(here("data", "plays.csv"))
players <- read_csv(here("data", "players.csv"))
tackles <- read_csv(here("data", "tackles.csv"))

tracking <- here("data") |> 
  list.files() |> 
  str_subset("tracking_week_") |> 
  str_c("data/", `...` = _) |> 
  map(read_csv) |> 
  list_rbind()

tracking <- tracking |>
  mutate(
    # make all plays go from left to right
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    # flip player direction and orientation
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o),
    dir_rad = pi * (dir / 180),
    # get orientation and direction in x and y direction
    # NA checks are for the ball
    dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
    dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),
    # Get directional speed/velo
    s_x = dir_x * s,
    s_y = dir_y * s,
    # Get directional acceleration
    a_x = dir_x * a,
    a_y = dir_y * a
  )

library(nflreadr)
pbp22 <- load_pbp(seasons = 2022)

# dictionary
# https://nflreadr.nflverse.com/articles/dictionary_pbp.html

# play_type ("run" includes scrambles)
# play_type_nfl
# rush_attempt: binary indicator for if the play was a run.

# quick check

# pbp22 |> 
#   select(old_game_id, play_id, play_type, rush_attempt) |> 
#   filter(rush_attempt == 1)
# 
# pbp22 |> 
#   select(old_game_id, play_id, play_type_nfl, rush_attempt) |> 
#   filter(rush_attempt == 1)

runs <- pbp22 |> 
  filter(play_type == "run") |> 
  distinct(gameId = as.double(old_game_id), 
           playId = as.double(play_id))

rb_bc_plays <- plays |> 
  left_join(select(players, nflId, position), by = c("ballCarrierId" = "nflId")) |> 
  filter(position == "RB") |> 
  select(gameId, playId, ballCarrierDisplayName, position)

# check/correct start and end frames

# 5539 total plays

# end of play event
end_event <- tracking |> 
  inner_join(runs) |> 
  distinct(gameId, playId, frameId, event) |> 
  inner_join(rb_bc_plays) |> 
  filter(event %in% c("fumble", "fumble_defense_recovered", "fumble_offense_recovered",
                      "out_of_bounds", "safety", "tackle", "touchdown")) |>
  # count(gameId, playId, sort = TRUE)
  # correct 1 play where there's a safety after a tackle
  group_by(gameId, playId) |> 
  slice_min(frameId) |> 
  select(gameId, playId, end_frame = frameId)

# start of play event
start_event <- tracking |> 
  inner_join(runs) |> 
  distinct(gameId, playId, frameId, event) |> 
  inner_join(rb_bc_plays) |> 
  filter(event %in% c("ball_snap", "autoevent_ballsnap", "snap_direct")) |> 
  group_by(gameId, playId) |> 
  slice_min(frameId) |> 
  distinct(gameId, playId, start_frame = frameId)

frames <- start_event |> 
  full_join(end_event)

# for future use
# write_csv(frames, here("data", "frames.csv"))


# main data
# tracking data for all RB run plays
# frames between snap and end-of-play event
# momentum calculated for all players
tracking_runs <- tracking |> 
  inner_join(frames) |> 
  filter(frameId >= start_frame, frameId <= end_frame) |> 
  left_join(select(players, nflId, weight)) |> 
  mutate(m_x = s_x * weight,
         m_y = s_y * weight)

