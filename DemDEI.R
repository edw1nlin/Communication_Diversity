library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(readr)

DemDEI <- read.csv("/Users/edwinlam/Desktop/Zollman/DemDEI/Dem.csv",
                   skip = 6,
                   header = TRUE,
                   stringsAsFactors = FALSE,
                   check.names = FALSE) %>%
  mutate(
    result = case_when(
      `successful-learning?` == "false" ~ 0,
      TRUE ~ 1
    )
  )

Homo <- read.csv("/Users/edwinlam/Desktop/Zollman/DemDEI/HomoDem.csv",
                 skip = 6,
                 header = TRUE,
                 stringsAsFactors = FALSE,
                 check.names = FALSE) %>%
  mutate(
    result = case_when(
      `successful-learning?` == "false" ~ 0,
      TRUE ~ 1
    )
  )

Dem_models <- DemDEI %>%
  group_by(`num-turtles`, `network-structure`) %>%
  summarise(
    prob = mean(result, na.rm = TRUE),
    time = mean(`[step]`, na.rm = TRUE),
    .groups = "drop"
  )

HomoDem_models <- Homo %>%
  group_by(`num-turtles`, `network-structure`) %>%
  summarise(
    prob = mean(result, na.rm = TRUE),
    time = mean(`[step]`, na.rm = TRUE),
    .groups = "drop"
  )

Dem_combined <- Dem_models %>%
  select(
    `num-turtles`,
    `network-structure`,
    prob_30 = `prob`,
    time_30 = `time`
  ) %>%
  inner_join(
    HomoDem_models %>%
      select(
        `num-turtles`,
        `network-structure`,
        prob_0 = `prob`,
        time_0 = `time`
      ),
    by = c("num-turtles", "network-structure")
  ) %>%
  mutate(
    prob_difference = (prob_30 - prob_0),
    abs_prob_difference = abs(prob_difference),
    time_difference = time_30 - time_0
  )


Dem_combined %>%
    filter(case_when(
      `network-structure` == "cycle" ~ `num-turtles` >= 4,
      `network-structure` == "wheel" ~ `num-turtles` >= 5,
      `network-structure` == "complete" ~ `num-turtles` >= 5,
      TRUE ~ FALSE
  )) %>%
  ggplot(
    aes(
      x = `num-turtles`,
      y = prob_difference,
      color = `network-structure`,
      group = `network-structure`
    )
  ) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_color_manual(
    values = c(
      "cycle" = "green",
      "wheel" = "blue",
      "complete" = "red"
    ),
    breaks = c("cycle", "wheel", "complete"),
    labels = c(
      "Cycle",
      "Wheel",
      "Complete"
    ),
    name = ""
  ) +
  scale_x_continuous(breaks = seq(2, 12, 2)) +
  labs(
    x = "Size",
    y = "Difference in Probability of Successful Learning"
  ) +
  theme(
    legend.position = c(0.1, 0.9),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
ggsave("DemDiff.png", width = 8.5, height = 7, dpi = 300)