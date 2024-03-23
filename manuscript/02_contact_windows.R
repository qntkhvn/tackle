library(here)

source(here("manuscript", "01_data_prep.R"))
tracking_bc <- tracking_runs |>
  left_join(select(plays, gameId:ballCarrierId)) |>
  filter(nflId == ballCarrierId) |>
  select(gameId, playId, frameId,
         club_bc = club, x_bc = x, y_bc = y)

tracking_dist_bc <- tracking_runs |>
  left_join(tracking_bc) |>
  filter(club != club_bc, club != "football") |>
  mutate(dist_bc = sqrt((x - x_bc) ^ 2 + (y - y_bc) ^ 2))


# tracking_dist_bc <- read_csv(here("data", "tracking_dist_bc.csv.gz"))

# closest distance distribution

hist_dist_bc <- tracking_dist_bc |> 
  filter(event == "first_contact") |> 
  group_by(gameId, playId) |> 
  slice_min(dist_bc) |> 
  bind_rows(
    tracking_dist_bc |> 
      filter(event == "tackle") |> 
      group_by(gameId, playId) |> 
      slice_min(dist_bc) |> 
      filter(dist_bc <= 5)
  ) |> 
  ggplot(aes(dist_bc, fill = event)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.7) +
  geom_vline(xintercept = 1.5, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 4, 0.5)) +
  labs(x = "Distance between ball-carrier and closest defender (yards)",
       y = "Frequency",
       fill = "Event") +
  theme(panel.grid.minor = element_blank(),
        legend.key.height= unit(0.5, "cm"),
        legend.key.width= unit(0.5, "cm"))


contact_frames_n_def <- tracking_dist_bc |> 
  filter(dist_bc <= 1.5) |> 
  distinct(gameId, playId, frameId, nflId) |> 
  group_by(gameId, playId, frameId) |> 
  summarize(n_def = n_distinct(nflId)) |> 
  ungroup()

contact_frames <- tracking_dist_bc |> 
  filter(dist_bc <= 1.5) |> 
  distinct(gameId, playId, frameId) |> 
  arrange(gameId, playId, frameId)

contact_windows <- contact_frames |> 
  group_by(gameId, playId) |> 
  mutate(diff = frameId - lag(frameId)) |> 
  mutate(diff = ifelse(diff == 1, 0, diff),
         diff = ifelse(diff > 1, 1, diff),
         diff = ifelse(is.na(diff), 1, diff),
         window = cumsum(diff)) |> 
  ungroup() |> 
  select(-diff)


# contact_windows |> 
#   distinct(gameId, playId, window) |> 
#   nrow()

p1 <- contact_windows |> 
  group_by(gameId, playId, window) |> 
  summarize(start = min(frameId) / 10,
            end = max(frameId) / 10,
            len = end - start) |> 
  mutate(lab = "Contact window length") |> 
  ggplot(aes(len)) +
  geom_histogram(fill = "gray", color = "white") +
  labs(x = "Time (seconds)",
       y = "Frequency") +
  facet_wrap(~ lab)


# distribution for number of windows
p2 <- contact_windows |> 
  distinct(gameId, playId, window) |> 
  group_by(gameId, playId) |> 
  slice_max(window) |> 
  ungroup() |> 
  count(window) |> 
  mutate(lab = "Contact windows per play") |> 
  ggplot(aes(window, n)) +
  geom_col(fill = "gray") +
  labs(x = "Windows",
       y = NULL) +
  facet_wrap(~ lab) +
  theme(panel.grid.minor = element_blank())

# distribution of number of defenders per contact window
p3 <- tracking_dist_bc |> 
  filter(dist_bc <= 1.5) |> 
  inner_join(contact_windows) |> 
  # distinct(gameId, playId, nflId, frameId, window) |> 
  # arrange(gameId, playId, nflId) |> 
  group_by(gameId, playId, window) |> 
  summarize(n_def = n_distinct(nflId)) |> 
  ungroup() |> 
  count(n_def) |> 
  mutate(lab = "Defenders per window") |> 
  ggplot(aes(n_def, n)) +
  geom_col(fill = "gray") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Defenders",
       y = NULL) +
  facet_wrap(~ lab) +
  theme(panel.grid.minor = element_blank())

window_summary <- cowplot::plot_grid(p1, p2, p3, nrow = 1)






