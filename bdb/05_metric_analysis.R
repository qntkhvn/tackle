library(here)
source(here("code", "01_data_prep.R"))

# ranking -----------------------------------------------------------------

tack_sum <- read_csv(here("data", "partial_tackles_summary.csv")) # from 04_momentum_credit.R

clubs <- tracking |> 
  filter(nflId %in% tack_sum$nflId) |> 
  distinct(nflId, club) |> 
  # handle players that switch teams
  group_by(nflId) |> 
  summarize(club = str_c(club, collapse = "/"))

library(gt)

rb_runs_counts <- tracking |> 
  filter(nflId %in% tack_sum$nflId) |> 
  inner_join(distinct(frames, gameId, playId)) |> 
  group_by(nflId) |> 
  summarize(n_rb_runs = n_distinct(gameId, playId))

tack_sum <- tack_sum |> 
  inner_join(rb_runs_counts) |> 
  mutate(avg_across_runs = sum_tackle_cre / n_rb_runs)


leaders <- tack_sum |> 
  head(20) |> 
  arrange(-sum_tackle_cre) |> 
  left_join(clubs) |> 
  select(displayName, club, position, n_rb_runs, n_windows, sum_tackle_cre, avg_across_runs) |>
  mutate(displayName = str_replace(displayName, "Sebastian Joseph", "Sebastian Joseph-Day")) |> 
  gt() |> 
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) |>
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) |>
  fmt_number(
    columns = c(sum_tackle_cre, avg_across_runs),
    decimals = 2,
  ) |>
  data_color(
    columns = c(sum_tackle_cre),
    colors = scales::col_numeric(
      palette = c("#FEE0D2", "#67000D"),
      domain = NULL
    )
  ) |> 
  data_color(
    columns = c(avg_across_runs),
    colors = scales::col_numeric(
      palette = c("#DEEBF7", "#08306B"),
      domain = NULL
    )
  ) |> 
  cols_label(
    displayName = md("**Player**"),
    position = md("**Position**"),
    club = md("**Team**"),
    n_rb_runs = md("**Plays**"),
    n_windows = md("**Contact Windows**"),
    sum_tackle_cre = md("**Total MFTs**"),
    avg_across_runs = md("**Average MFTs**"),
  ) |> 
  cols_align(
    align = "center",
    columns = n_rb_runs:avg_across_runs
  ) |> 
  tab_options(
    table.border.top.style = "a"
  ) |> 
  tab_header(md("**Momentum-Based Fractional Tackles (MFTs) Leaders**"),
             md("Summarized across all RB run plays (Weeks 1-9, 2022 NFL season)")) |> 
  tab_style(style = cell_borders(sides = "top"),
            locations = cells_title("title"))

leaders

# gtsave(leaders, "leaders.png")


# correlation -------------------------------------------------------------

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
  geom_point(aes(color = pos_group), alpha = 0.6, size = 1.2) +
  geom_abline(linewidth = 0.6, color = "black", linetype = "dashed", alpha = 0.7) +
  scale_color_manual(values = c("#1E88E5", "#FFC107", "#004D40")) +
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  labs(
    x = "Total tackles + assists/2",
    y = "Total MFTs",
    color = "Position",
    title = "Momentum-based fractional tackles vs. tackles + assists/2 (r = 0.93)",
    subtitle = "Dashed line: identity (45°) line"
  ) +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10.5),
        axis.text = element_text(size = 9),
        legend.margin = unit(-2, "cm"),
        legend.box.margin = margin(0, 0, -1, -1),
        legend.text = element_text(size = 9.5),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Chivo"))
