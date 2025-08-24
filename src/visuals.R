individual_group_facets <- function(data, sex, group_name) {

  data <- copy(data)

  keep <- data.table(SEX = sex, GROUP_NAME = group_name)
  data[keep, flag := TRUE, on = c(sex = "SEX", group_name = "GROUP_NAME")]

  data[flag == TRUE & name == group_name] %>%
    ggplot(
      mapping = aes(
        x = year
      )
    ) +
    geom_line(
      mapping = aes(
        y = group_prc_year,
        color = sex
      ),
      alpha = 0.25
    ) +
    geom_line(
      mapping = aes(
        y = group_prc_year_ma,
        color = sex
      ),
    ) +
    scale_x_continuous(
      name = NULL
    ) +
    scale_y_continuous(
      name = NULL,
      label = label_percent(),
      limits = c(0, NA)
    ) +
    scale_color_manual(
      values = c(
        F = "deeppink",
        M = "darkblue"
      ),
      guide = NULL
    ) +
    facet_wrap(
      facets = vars(group_label)
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(
        size = rel(1.5),
        face = "bold"
      )
    )

}
