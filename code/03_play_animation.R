library(here)
source(here("code", "01_data_prep.R"))
source(here("code", "02_contact_windows.R"))


window_indicator <- tracking_dist_bc |>
  filter(gameId == 2022091108, playId == 1333) |> 
  filter(dist_bc <= 1.5) |> 
  select(gameId, playId, nflId, frameId) |> 
  mutate(i_window = 1)

ex <- tracking |> 
  filter(gameId == 2022091108, playId == 1333) |>
  left_join(window_indicator) |> 
  mutate(
    pt_color = case_when(
      club == "TEN" & is.na(i_window) ~ "#4B92DB",
      club == "TEN" & i_window == 1 ~ "gold",
      club == "NYG" & nflId != 46071 ~ "#A71930",
      club == "NYG" & nflId == 46071 ~ "#A71930",
      club == "football" ~ "white"
    ),
    pt_size = case_when(
      club == "TEN" ~ 2.8,
      club == "NYG" ~ 2.8,
      club == "football" ~ 1.4
    )
  )


desc <- plays |> 
  filter(gameId == 2022091108, playId == 1333) |> 
  pull(playDescription) |> 
  str_replace("\\)\\.", "\\)")

library(gganimate)

anim <- ggplot()  +
  annotate("text", 
           x = seq(70, 90, 10),
           y = 10,
           color = "#bebebe",
           family = "Chivo",
           label = 10 * c(4:2)) +
  annotate("text", 
           x = seq(70, 90, 10),
           y = 40,
           color = "#bebebe",
           family = "Chivo",
           label = 10 * c(4:2),
           angle = 180) +
  annotate("text", 
           x = setdiff(seq(65, 95, 1), seq(65, 95, 5)),
           y = 0,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(65, 95, 1), seq(65, 95, 5)),
           y = 160 / 3,
           color = "#bebebe",
           label = "—",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(65, 95, 1), seq(65, 95, 5)),
           y = 23.36667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("text", 
           x = setdiff(seq(65, 95, 1), seq(65, 95, 5)),
           y = 29.96667,
           color = "#bebebe",
           label = "–",
           angle = 90) +
  annotate("segment", 
           x = 15,
           xend = 65,
           y = c(-Inf, Inf),
           yend = c(-Inf, Inf),
           color = "#bebebe") +
  geom_vline(xintercept = seq(65, 95, 5), color = "#bebebe") +
  geom_point(data = filter(ex, frameId %in% 6:64), shape = 21,
             aes(120 - x, 160 / 3 - y, size = pt_size, fill = pt_color)) +
  scale_size_identity() +
  scale_fill_identity() +
  transition_time(frameId) +
  ease_aes("linear") +
  coord_cartesian(xlim = c(65, 95), ylim = c(0, 160 / 3), expand = FALSE) +
  theme_minimal() +
  labs(title = "<span style = 'color:#A71930;'>**New York Giants**</span> vs. <span style = 'color:#4B92DB;'>**Tennessee Titans**</span>, 2022 NFL Week 1",
       subtitle = str_c("Q4: ", desc)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
        plot.title = ggtext::element_markdown(hjust = 0.5, size = 12),
        text = element_text(family = "Chivo", color = "#26282A"),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

a1 <- animate(
  anim,
  width = 520,
  height = 320,
  duration = 6.3,
  fps = 10,
  end_pause = 4,
  res = 105,
)

a1

# line plot ---------------------------------------------------------------

# these save the day
# https://stackoverflow.com/questions/67023114/how-do-i-make-annotations-appear-at-a-specific-frame-using-gganimate
# https://stackoverflow.com/questions/76941748/how-to-animate-geom-segment-in-gganimate-chart?noredirect=1&lq=1

#38-41
#47-50
#52-64

anm <- tracking_runs |> 
  left_join(select(plays, gameId:ballCarrierId)) |> 
  filter(nflId == ballCarrierId) |> 
  filter(gameId == 2022091108, playId == 1333) |> 
  left_join(contact_windows) |> 
  mutate(
    xmin_rect = case_when(
      window == 1 & frameId == 38 ~ 32,
      window == 2 & frameId == 47 ~ 41,
      window == 3 & frameId == 52 ~ 46,
      is.na(window) ~ 0,
      TRUE ~ 0,
    ),
    xmax_rect = case_when(
      window == 1 & frameId == 38 ~ 35,
      window == 2 & frameId == 47 ~ 44,
      window == 3 & frameId == 52 ~ 58,
      is.na(window) ~ 0,
      TRUE ~ 0,
    )
  ) |>
  mutate(frameId = frameId - 6) |> 
  ggplot(aes(frameId, m_x)) +
  geom_rect(aes(xmin = xmin_rect, xmax = xmax_rect, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("gold", 0.6)) +
  geom_line(alpha = 0.8) +
  scale_fill_identity() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(0, 60, 10), labels = 0:6) +
  expand_limits(x = 60) +
  labs(
    x = "Time since snap (seconds)",
    y = "\n\nMomentum in endzone direction"
  ) +
  transition_manual(frameId, cumulative = TRUE) +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9.5),
        text = element_text(family = "Chivo"))

a2 <- animate(
  anm,
  width = 500,
  height = 270,
  fps = 10,
  end_pause = 4,
  res = 105
)

library(magick)
g1 <- image_read(a1)
g2 <- image_read(a2)

length(g1)
length(g2)

comb <- image_append(c(g1[1], g2[1]), stack = TRUE)
for(i in 2:59){
  combined <- image_append(c(g1[i], g2[i]), stack = TRUE)
  comb <- c(comb, combined)
}
comb

# g2[length(g2)]


# for kaggle plotly

# mmt_df <- tracking_runs |> 
#   left_join(select(plays, gameId:ballCarrierId)) |> 
#   filter(nflId == ballCarrierId) |> 
#   filter(gameId == 2022091108, playId == 1333) |> 
#   select(Frame = frameId,
#          Momentum = m_x) |> 
#   mutate(Momentum = round(Momentum, 3))
# 
# write_csv(mmt_df, here("data", "barkley.csv"))