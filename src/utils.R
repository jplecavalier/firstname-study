group_details <- function(data) {

  data[group_name_nb > 1L, .(
    name = name,
    nb = nb,
    group_prc_name = nb / group_nb,
    group_name_nb = group_name_nb
  ), .(sex, year, group_name)]

}

summary_all_years <- function(data) {

  data <- copy(data)

  data[, .(
    nb = sum(nb),
    min_prc_year_ma = min(prc_year_ma, na.rm = TRUE),
    max_prc_year_ma = max(prc_year, na.rm = TRUE),
    range_prc_year_ma = max(prc_year, na.rm = TRUE) - min(prc_year, na.rm = TRUE),
    mean_prc_year_ma = mean(prc_year, na.rm = TRUE),
    var_prc_year_ma = var(prc_year, na.rm = TRUE),
    cv_prc_year_ma = sd(prc_year, na.rm = TRUE) / mean(prc_year, na.rm = TRUE),
    group_nb = sum(group_nb),
    group_min_prc_year_ma = min(group_prc_year_ma, na.rm = TRUE),
    group_max_prc_year_ma = max(group_prc_year, na.rm = TRUE),
    group_range_prc_year_ma = max(group_prc_year, na.rm = TRUE) - min(group_prc_year, na.rm = TRUE),
    group_mean_prc_year_ma = mean(group_prc_year, na.rm = TRUE),
    group_var_prc_year_ma = var(group_prc_year, na.rm = TRUE),
    group_cv_prc_year_ma = sd(group_prc_year, na.rm = TRUE) / mean(group_prc_year, na.rm = TRUE)
  ), .(sex, name, group_name, group_name_nb, group_label)][, `:=`(
    prc = nb / sum(nb),
    group_prc = group_nb / sum(nb)
  ), .(sex)][]

}

filter_gen <- function(data, year_begin, year_end, top = 5L) {

  data <- copy(data)

  gen_summary <- data[year >= year_begin & year <= year_end, .(
    nb = sum(nb)
  ), .(sex, group_label)][, .(
    group_label = group_label,
    nb_gen = nb,
    rnk_gen = rank(-nb, ties.method = "min")
  ), .(sex)][rnk_gen <= top]

  gen_data <- data[gen_summary, `:=`(
    flag = TRUE,
    rnk_gen = rnk_gen
  ), on = .(sex, group_label)]
  gen_data[flag == TRUE][, flag := NULL][]

}

drop_namesake_group <- function(data, prc, nb) {

  group_details(data)[, .(
    nb = sum(nb)
  ), .(sex, group_name, name)][, `:=`(
    group_nb = sum(nb)
  ), .(sex, group_name)][, .(
    max_prc_name = max(nb / group_nb),
    nb_name = .N
  ), .(sex, group_name)][!(max_prc_name >= prc & nb_name <= nb), .(sex, group_name, nb_name, max_prc_name)]

}
