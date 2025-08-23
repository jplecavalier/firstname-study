library(data.table)
library(ggplot2)
library(ragg)

data <- fread("data/shower-poll-results.csv")

ggplot(data) +
  geom_bar(
    mapping = aes(
      x = rang
    ),
    color = "deeppink",
    fill = "pink"
  ) +
  scale_x_continuous(
    name = "Rang"
  ) +
  scale_y_continuous(
    name = NULL
  ) +
  facet_wrap(
    facets = vars(prenom)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(
      size = rel(1.5),
      face = "bold"
    )
  )

ggsave(
  filename = "poll-results.png",
  device = agg_png,
  scale = 2L,
  width = 2560L,
  height = 1600L,
  dpi = 320L,
  units = "px",
  bg = "white"
)

copy(data)[data.table(
  rang = 1:4,
  score = c(10, 6, 3, 1)
), `:=`(
  score = score
), on = .(rang)][, .(
  score = sum(score)
), .(prenom)][order(-score)]
