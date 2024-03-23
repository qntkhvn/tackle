# Load the packages
library(tidyverse)
library(here)
library(nflreadr)

# Import Data
games_data <- read_csv(here("data", "games.csv"))
plays_data <- read_csv(here("data", "plays.csv"))
players_data <- read_csv(here("data", "players.csv"))
tackles_data <- read_csv(here("data", "tackles.csv"))

tracking <- here("data") |> 
  list.files() |> 
  str_subset("tracking_week_") |> 
  str_c("data/", `...` = _) |> 
  map(read_csv) |> 
  list_rbind()

# Convert all plays to one direction
tracking <- tracking %>% 
  mutate(
    # make all plays go from left to right
    x = ifelse(playDirection == "left",120-x,x),
    y = ifelse(playDirection == "left",160/3-y,
               y),
    dir = ifelse(playDirection == "left",dir+180,
                 dir),
    dir = ifelse(dir>360,dir-360,dir),
    o = ifelse(playDirection == "left",o+180,o),
    o = ifelse(o>360,o-360,o)
  ) %>% 
  mutate(dir_rad = pi*(dir/180),
         # get oreination and direction in x and y direction
         # NA checks are for the ball
         dir_x = ifelse(is.na(dir),NA_real_,sin(dir_rad)),
         dir_y = ifelse(is.na(dir),NA_real_,cos(dir_rad)),
         # Get directional speed/velo
         s_x = dir_x * s,
         s_y = dir_y*s,
         # Get directional acceleration
         a_x = dir_x *a,
         a_y = dir_y*a,
         # Concatenate playID and gameID
         gamePlay_Id = paste(gameId,playId,sep="-"))

# Extract Run Plays
pbp22 <- load_pbp(seasons = 2022)
runs <- pbp22 %>% 
  filter(play_type == "run") %>% 
  distinct(gameId = as.double(old_game_id), 
           playId = as.double(play_id))

# only focus on running back run play
rb_id <- players_data %>% 
  filter(position == "RB") %>% 
  select(nflId,displayName)

tracking_runs <- tracking %>%  
  inner_join(runs) %>%  
  # filter out frames before snap
  # first 5 frames are before snap
  filter(frameId > 5) %>% 
  left_join(select(players_data, nflId, weight)) %>%  
  mutate(m_x = s_x * weight,
         m_y = s_y * weight,
         gamePlay_Id = paste(gameId,playId,sep="-"))

# Extracting Contact Window
rb_id <- players_data %>% 
  filter(position == "RB") %>% 
  select(nflId,displayName)



tracking_bc <- tracking_runs %>%  
  left_join(select(plays_data, gameId:ballCarrierId)) %>%  
  filter(nflId == ballCarrierId,nflId %in% rb_id$nflId) %>% 
  select(gamePlay_Id, frameId, 
         club_bc = club, x_bc = x, y_bc = y,s_bc = s_x)

# see 01_data_prep.R
# frames data were obtained there
frames <- read_csv(here("data", "frames.csv"))


# Get the last frame before the play called dead
last_non_na_event<- frames %>% 
  mutate(gamePlay_Id= paste(gameId,playId,sep="-")) %>% 
  select(gamePlay_Id,last_event_frame_Id=end_frame)

tracking_dist_bc <- tracking_runs %>% 
  filter(gamePlay_Id %in% tracking_bc$gamePlay_Id)%>% 
  left_join(tracking_bc) %>% 
  left_join(last_non_na_event,by="gamePlay_Id") %>% 
  filter(club != club_bc, club != "football",
         frameId<=last_event_frame_Id) %>% # this step removes everything after the last event 
  mutate(dist_bc = sqrt((x - x_bc) ^ 2 + (y - y_bc) ^ 2))

# calculate the amount of defenders in the frame and who are they
contact_frames_n_def <- tracking_dist_bc %>%  
  filter(dist_bc <= 1.5) %>%  
  distinct(gamePlay_Id, frameId, nflId) %>%  
  group_by(gamePlay_Id, frameId) %>%  
  summarize(n_def = n_distinct(nflId),
            defenders = toString(nflId)) %>% 
  ungroup()

# Get the contact frames
contact_frames <- tracking_dist_bc %>%  
  filter(dist_bc <= 1.5) %>%  
  distinct(gamePlay_Id, frameId) %>%  
  arrange(gamePlay_Id, frameId)

# Defining contact windows
contact_windows <- contact_frames %>% 
  group_by(gamePlay_Id) %>%  
  mutate(diff = frameId - lag(frameId)) %>%  
  mutate(diff = ifelse(diff == 1, 0, diff),
         diff = ifelse(diff > 1, 1, diff),
         diff = ifelse(is.na(diff), 1, diff),
         window = cumsum(diff)
  ) %>%  
  ungroup() %>%  
  select(-diff) %>% 
  group_by(gamePlay_Id,window) %>% 
  mutate(frame_count = n()) %>% 
  filter(frame_count>1)


# see below if getting "Error: vector memory exhausted (limit reached?)"
# https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
max_velo_data <- tracking_dist_bc %>% 
  left_join(contact_windows,by="gamePlay_Id") %>% 
  group_by(gamePlay_Id,window) %>% 
  summarise(
    max_velo_before = ifelse(any(frameId.x<=last(frameId.y)),
                                 max(s_bc[frameId.x
                                          <=last(frameId.y)],
                                     na.rm=TRUE),NA),
    max_velo_before_frame = ifelse(any(frameId.x<last(frameId.y)),
                                       frameId.x[which.max(s_bc[frameId.x
                                                                <last(frameId.y)])],NA),
    max_velo_after = ifelse(any(frameId.x>=last(frameId.y)),
                                max(s_bc[frameId.x>=last(frameId.y)],
                                    na.rm=TRUE),NA)
  )



# credit assignment
tracking_bc <- tracking_runs %>%  
  left_join(select(plays_data, gameId:ballCarrierId)) %>%  
  filter(nflId == ballCarrierId,nflId %in% rb_id$nflId)

velo_val <- tracking_bc %>%  
  inner_join(contact_windows) %>%  
  group_by(gamePlay_Id, window) %>%
  #filter(first(s_x)>0) %>% 
  summarize(first_frame = first(frameId), last_frame = last(frameId),
            first_s_x = first(s_x),
            last_s_x = last(s_x),
            len = n()) %>%  
  ungroup() %>%
  inner_join(max_velo_data) %>%
  mutate(change = ifelse(
    last_s_x>=max_velo_after,
    ifelse(max_velo_before_frame<=first_frame,
           (first_s_x - last_s_x)/max_velo_before,
           (max_velo_before - last_s_x)/max_velo_before),
    ifelse(max_velo_after>max_velo_before,
           0,
           ifelse(max_velo_before_frame<=first_frame,
                  (first_s_x - max_velo_after)/max_velo_before,
                  (max_velo_before - max_velo_after)/max_velo_before)))
    ,frame_ini_val = change / len)

# break the table up to allows easier operations
expanded_contact_frames_n_def <- contact_frames_n_def %>% 
  separate_rows(defenders,sep=",\\s*") %>% 
  mutate(defenders = as.numeric(defenders))

velo_cre <- expanded_contact_frames_n_def %>%  
  left_join(contact_windows) %>% 
  left_join(select(velo_val, gamePlay_Id, window, frame_ini_val)) %>%  
  mutate(frame_final_val = frame_ini_val / n_def) %>%  
  select(gamePlay_Id:frameId, window, nflId = defenders, velo_cre = frame_final_val)

tracking_def <- tracking_runs %>%  
  inner_join(contact_windows) %>%  
  left_join(select(plays_data, gameId,playId, defensiveTeam)) %>%  
  filter(club == defensiveTeam) %>% 
  mutate(gamePlay_Id = paste(gameId,playId,sep="-"))

player_plays_keep <- tracking_dist_bc %>% 
  filter(dist_bc <= 1.5) %>% 
  distinct(gamePlay_Id, nflId)

tracking_cre <- tracking_def %>%  
  inner_join(velo_cre) %>%  
  inner_join(player_plays_keep)

# add all the credits together for each players
partial_tackles_summary <- tracking_cre %>%  
  group_by(gamePlay_Id, displayName, window,nflId) %>%  
  summarize(window_velo_cre = sum(velo_cre)) %>% 
  group_by(displayName,nflId) %>% 
  summarize(sum_tackle_cre = sum(window_velo_cre),
            n_windows = n()) %>% 
  #avg_tackle_cre = mean(window_velo_cre)) %>%  
  #filter(n_windows >= 50) %>% 
  arrange(desc(sum_tackle_cre)) %>%  
  left_join(select(players_data, displayName, position))

# write_csv(partial_tackles_summary, here("data", "partial_tackles_summary.csv"))
