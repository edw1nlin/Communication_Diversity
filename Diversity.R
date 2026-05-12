library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)
library(readr)

Diversity <- read.csv('/Users/edwinlam/Desktop/Zollman/Diversity/DIV50.csv',
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

DIV_models <- Diversity %>%
  group_by(`num-turtles`, `network-structure`) %>%
  summarise(
    prob = mean(result, na.rm = TRUE),
    time = mean(`[step]`, na.rm = TRUE),
    .groups = "drop"
  )


DIV_combined <- DIV_models %>%
  select(
    `num-turtles`,
    `network-structure`,
    prob_30 = `prob`,
    time_30 = `time`
  ) %>%
  inner_join(
    homo_models %>%
      select(
        `num-turtles`,
        `network-structure`,
        prob_0 = `prob`,
        time_0 = `time`
      ),
    by = c("num-turtles", "network-structure")
  ) %>%
  mutate(
    prob_difference = prob_30 - prob_0,
    abs_prob_difference = abs(prob_difference),
    time_difference = time_30 - time_0
  )

DIV_combined %>%
  filter(case_when(
    `network-structure` == "Cycle" ~ `num-turtles` >= 4,
    `network-structure` == "Wheel" ~ `num-turtles` >= 5,
    `network-structure` == "Complete" ~ `num-turtles` >= 3,
    TRUE ~ FALSE
  )) %>%
  ggplot(
    aes(
      x = `num-turtles`,
      y = prob_30,
      color = `network-structure`,
      group = `network-structure`
    )
  ) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Cycle" = "green",
      "Wheel" = "blue",
      "Complete" = "red"
    ),
    breaks = c("Cycle", "Wheel", "Complete"),
    labels = c(
      "Cycle",
      "Wheel",
      "Complete"
    ),
    name = ""
  ) +
  scale_x_continuous(breaks = seq(3, 12, 1)) +
  scale_y_continuous(breaks = seq(0.6, 1, 0.05))+
  labs(
    x = "Size",
    y = "Probability of Successful Learning"
  ) +
  theme(
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
ggsave("DIV50.png", width = 8.5, height = 7, dpi = 300)