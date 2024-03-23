

# example play ------------------------------------------------------------

library(here)
source(here("manuscript", "01_data_prep.R"))

# velo_df <- tracking_runs |>
#   left_join(select(plays, gameId:ballCarrierId)) |>
#   filter(nflId == ballCarrierId) |>
#   filter(gameId == 2022091108, playId == 1333) |>
#   select(Frame = frameId,
#          Velocity = s_x) |>
#   mutate(Velocity = round(Velocity, 3))
# 
# write_csv(velo_df, here("data", "velo.csv"))


ggplot() +
  geom_rect(aes(xmin = c(38, 47, 52), xmax = c(41, 50, 64), ymin = -Inf, ymax = Inf), fill = "#FFD70099") +
  geom_line(data = read_csv(here("data", "velo.csv")), 
            aes(Frame, Velocity), linewidth = 0.4, alpha = 0.8) +
  labs(x = "Frame", y = "Velocity toward the end zone") +
  geom_vline(xintercept = 45, linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = c(39.5, 48.5, 58), y = 2, label = paste0("(", 1:3, ")"), size = rel(3)) +
  scale_x_continuous(breaks = c(6, 38, 41, 45, 47, 50, 52, 64)) +
  theme(panel.grid.minor = element_blank())




# leaderboard -------------------------------------------------------------

tack_sum <- read_csv(here("data", "partial_tackles_summary.csv")) # from 04_momentum_credit.R

clubs <- tracking |> 
  filter(nflId %in% tack_sum$nflId) |> 
  distinct(nflId, club) |> 
  # handle players that switch teams
  group_by(nflId) |> 
  summarize(club = str_c(club, collapse = "/"))

rb_runs_counts <- tracking |> 
  filter(nflId %in% tack_sum$nflId) |> 
  inner_join(distinct(frames, gameId, playId)) |> 
  group_by(nflId) |> 
  summarize(n_rb_runs = n_distinct(gameId, playId))

tack_sum <- tack_sum |> 
  inner_join(rb_runs_counts) |> 
  mutate(avg_across_runs = sum_tackle_cre / n_rb_runs)


leaders <- tack_sum |> 
  head(15) |> 
  arrange(-sum_tackle_cre) |> 
  left_join(clubs) |> 
  select(displayName, club, position, n_rb_runs, n_windows, sum_tackle_cre, avg_across_runs)


# compare to tackles ------------------------------------------------------

cor_df <- tack_sum |> 
  left_join(
    tackles |>
      inner_join(distinct(frames, gameId, playId)) |>
      group_by(nflId) |>
      summarize(total_tackles = sum(tackle) + sum(assist) / 2)
  ) |> 
  filter(position %in% c("DB", "MLB", "NT", "SS", "FS", "ILB", "DE", "DT", "OLB", "CB")) |> 
  mutate(pos_group = case_when(
    position %in% c("DT", "NT", "DE") ~ "Defensive line",
    position %in% c("MLB", "OLB", "ILB") ~ "Linebackers",
    position %in% c("DB", "SS", "FS", "CB") ~ "Defensive backs"
  ))

cor(cor_df$total_tackles, cor_df$sum_tackle_cre, use = "complete.obs")

cor_df |> 
  ggplot(aes(total_tackles, sum_tackle_cre)) +
  geom_point(aes(color = pos_group), alpha = 0.4, size = 0.7) +
  geom_abline(linewidth = 0.6, color = "black", linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("#1E88E5", "#FFC107", "#8D0034")) +
  labs(
    x = "Total tackles + assists/2",
    y = "Total fractional tackles",
    color = "Position"
  )




# stability ---------------------------------------------------------------

source(here("manuscript", "03_credit.R"))

total_ft_first_last <- tracking_cre |> 
  left_join(select(games, gameId, week)) |> 
  mutate(i_week = ifelse(week < 5, "first", "last")) |> 
  group_by(nflId, i_week) |> 
  summarize(total_ft = sum(velo_cre)) |> 
  ungroup() |> 
  pivot_wider(names_from = i_week, values_from = total_ft) |> 
  left_join(select(players, nflId, position)) |> 
  filter(position %in% c("DB", "MLB", "NT", "SS", "FS", "ILB", "DE", "DT", "OLB", "CB")) |> 
  mutate(pos_group = case_when(
    position %in% c("DT", "NT", "DE") ~ "Defensive line",
    position %in% c("MLB", "OLB", "ILB") ~ "Linebackers",
    position %in% c("DB", "SS", "FS", "CB") ~ "Defensive backs"
  ))

tackles_first_last <- tackles |>
  inner_join(distinct(frames, gameId, playId)) |>
  left_join(select(games, gameId, week)) |> 
  mutate(i_week = ifelse(week < 5, "first", "last")) |> 
  group_by(nflId, i_week) |>
  summarize(total_tackles = sum(tackle) + sum(assist) / 2) |> 
  ungroup() |> 
  pivot_wider(names_from = i_week, values_from = total_tackles) |> 
  left_join(select(players, nflId, position)) |> 
  filter(position %in% c("DB", "MLB", "NT", "SS", "FS", "ILB", "DE", "DT", "OLB", "CB")) |> 
  mutate(pos_group = case_when(
    position %in% c("DT", "NT", "DE") ~ "Defensive line",
    position %in% c("MLB", "OLB", "ILB") ~ "Linebackers",
    position %in% c("DB", "SS", "FS", "CB") ~ "Defensive backs"
  ))

tackles_first_last |> 
  mutate(type = "Total tackles + assists / 2") |> 
  bind_rows(
    total_mft_first_last |> 
      mutate(type = "Total MFTs")
  ) |> 
  ggplot(aes(first, last, color = pos_group)) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_color_manual(values = c("#1E88E5", "#FFC107", "#004D40")) +
  facet_grid(vars(type), vars(pos_group), scales = "free") +
  theme(legend.position = "none") +
  labs(x = "First 4 weeks",
       y = "Last 5 weeks") +
  theme(axis.title = element_text(size = 17, family = "Chivo"),
        strip.text = element_text(size = 17, family = "Chivo"),
        axis.text = element_text(size = 14, family = "Chivo"))











# 0.691
cor(total_mft_first_last$first, 
    total_mft_first_last$last,
    use = "complete.obs")

total_mft_first_last |> 
  ggplot(aes(first, last)) +
  geom_point(aes(color = pos_group), alpha = 0.6, size = 1.2) +
  scale_color_manual(values = c("#1E88E5", "#FFC107", "#004D40")) +
  theme(legend.position = "bottom") +
  labs(color = "Position",
       x = "Total MFTs (first 4 weeks)",
       y = "Total MFTs (last 5 weeks)") +
  facet_wrap(~ pos_group)

tackles_first_last |>
  mutate(type = "Total tackles + assists/2") |>
  bind_rows(
    total_mft_first_last |>
      mutate(type = "Total fractional tackles")
  ) |> 
  mutate(facet = pos_group) |> 
  bind_rows(
    read_csv(here("data", "stability.csv")) |> 
      mutate(facet = "Overall")
  ) |> 
  mutate(corr = case_when(
    facet == "Defensive backs" & type == "Total fractional tackles" ~ 0.56,
    facet == "Defensive backs" & type == "Total tackles + assists/2" ~ 0.46,
    facet == "Defensive line" & type == "Total fractional tackles" ~ 0.58,
    facet == "Defensive line" & type == "Total tackles + assists/2" ~ 0.51,
    facet == "Linebackers" & type == "Total fractional tackles" ~ 0.73,
    facet == "Linebackers" & type == "Total tackles + assists/2" ~ 0.64,
    facet == "Overall" & type == "Total fractional tackles" ~ 0.69,
    facet == "Overall" & type == "Total tackles + assists/2" ~ 0.59
  )) |> 
  mutate(corr = str_c("r = ", corr)) |> 
  group_by(facet, type) |> 
  mutate(o = row_number()) |> 
  ungroup() |> 
  mutate(corr = ifelse(o > 1, NA, corr)) |> 
  filter(first >= 0 & last >= 0) |> 
  mutate(facet = fct_relevel(facet, "Overall", after = 0)) |> 
  ggplot() +
  geom_point(aes(first, last, color = pos_group), alpha = 0.4, size = 1) +
  geom_text(aes(label = corr), y = 0.75, x = 18.5, size = rel(2.7), color = "black", alpha = 0.8) +
  scale_color_manual(values = c("#1E88E5", "#FFC107", "#8D0034")) +
  facet_grid(vars(type), vars(facet), scales = "free_y") +
  theme(legend.position = "bottom",
        legend.margin = margin(-5, 15, 5, 0),
        panel.grid.minor = element_blank()) +
  labs(x = "First 4 weeks",
       y = "Last 5 weeks",
       color = "Position")
