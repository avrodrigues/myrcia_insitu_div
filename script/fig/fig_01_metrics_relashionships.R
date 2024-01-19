
# load package ------------------------------------------------------------

library(tidyverse)
library(here)
library(ggdist)


# load data ---------------------------------------------------------------

evo_metrics_df <- readRDS(here("output", "evo_metrics_df.rds"))


# define colors -----------------------------------------------------------

colors_evo <-  c(
  "#363870",
  "#589eab",
  "#d3a838",
  "#362401",
  "#BB4455"
)

greys <- c(
  "#040400",
  "#1F1F1B",
  "#3B3B37",
  "#575753",
  "#73736F",
  "#8E8E8A",
  "#AAAAA6",
  "#C6C6C2",
  "#E2E2DE",
  "#FEFEFA"
)

bg <- "#FAF8F4"


# explore relationships ---------------------------------------------------

theme_set(theme_bw())


##|- distribution in each evoregion ----
evo_metrics_df %>% 
  ggplot(
    aes(
      x = age_halfedge, 
      y = fct_rev(evoregion), 
      fill = evoregion
      )
    ) +
  stat_dist_halfeye(alpha = 0.8) +
  scale_fill_manual(values = colors_evo, guide = "none") +
  labs(
    title = "Average age of assembly", 
    x = "Mean age", 
    y = "Evoregions"
  )

evo_metrics_df %>% 
  ggplot(
    aes(
      x = div_jetz, 
      y = fct_rev(evoregion), 
      fill = evoregion
    )
  ) +
  stat_dist_halfeye(alpha = 0.8) +
  scale_fill_manual(values = colors_evo, guide = "none") +
  labs(
    title = "Diversification rates", 
    x = "DR", 
    y = "Evoregions"
  )

evo_metrics_df %>% 
  ggplot(
    aes(
      x = div_mb_jetz, 
      y = fct_rev(evoregion), 
      fill = evoregion
    )
  ) +
  stat_dist_halfeye(alpha = 0.8) +
  scale_fill_manual(values = colors_evo, guide = "none") +
  labs(
    title = "In situ Diversification rates", 
    x = "in situ DR", 
    y = "Evoregions"
  )

evo_metrics_df %>% 
  ggplot(
    aes(
      x = div_insitu_prop, 
      y = fct_rev(evoregion), 
      fill = evoregion
    )
  ) +
  stat_dist_halfeye(alpha = 0.8) +
  scale_fill_manual(values = colors_evo, guide = "none") +
  labs(
    title = "Propotional In situ Diversification rates", 
    x = "in situ DR", 
    y = "Evoregions"
  )


evo_metrics_df %>% 
  ggplot(
    aes(
      x = ses_mpd, 
      y = fct_rev(evoregion), 
      fill = evoregion
    )
  ) +
  stat_dist_halfeye(alpha = 0.8) +
  scale_fill_manual(values = colors_evo, guide = "none") +
  labs(
    title = "Phylogenetic diversity", 
    x = "SES MPD", 
    y = "Evoregions"
  )

#- relationships among metrics ----


evo_metrics_df %>% 
  ggplot(aes(x = age_halfedge, y = div_jetz, color = evoregion)) +
  scale_color_manual(values = colors_evo, name = "Evoregions") +
  geom_point() +
  labs(x = "mean age", y = "Diversification rate")


evo_metrics_df %>% 
  ggplot(aes(x = age_halfedge, y = div_insitu_prop, color = evoregion)) +
  scale_color_manual(values = colors_evo, name = "Evoregions") +
  geom_point() +
  labs(x = "mean age", y = "Proportional In situ DR")     

evo_metrics_df %>% 
  ggplot(aes(x = age_halfedge, y = ses_mpd, color = evoregion)) +
  scale_color_manual(values = colors_evo, name = "Evoregions") +
  geom_point() +
  labs(x = "mean age", y = "SES mpd")

evo_metrics_df %>% 
  ggplot(aes(x = ses_mpd, y = div_jetz, color = evoregion)) +
  scale_color_manual(values = colors_evo, name = "Evoregions") +
  geom_point() +
  labs(x = "SES mpd", y = "Diversification rate")

evo_metrics_df %>% 
  ggplot(aes(x = ses_mpd, y = div_insitu_prop, color = evoregion)) +
  geom_point() +
  scale_color_manual(values = colors_evo, name = "Evoregions") +
  labs(x = "SES mpd", y = "Proportional In situ DR")   

evo_metrics_df %>% 
  ggplot(aes(x = div_jetz, y = div_insitu_prop, color = evoregion)) +
  geom_point() +
  scale_color_manual(values = colors_evo, name = "Evoregions") +
  labs(x = "Diversification rate", y = "Proportional In situ DR")     


cor.test(evo_metrics_df$age_halfedge,
         evo_metrics_df$div_insitu_prop, 
         method = "spearman") %>% broom::tidy()

cor.test(evo_metrics_df$age_recent, 
         evo_metrics_df$div_insitu_prop, 
         method = "spearman") %>% broom::tidy()

cor.test(evo_metrics_df$ses_mpd,
         evo_metrics_df$div_insitu_prop, 
         method = "spearman") %>% broom::tidy()

cor.test(evo_metrics_df$age_halfedge, 
         evo_metrics_df$ses_mpd, 
         method = "spearman") %>% broom::tidy()






