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

filter_peak <- function(data, year_begin, year_end, top_years = 3L, min_group_peak_prc = 0.005) {

  data <- copy(data)

  # TODO: Make sure to use all the data for calculating prc instead of filtered data

  peak_groups <- data[year >= year_begin & year <= year_end, .(
    group_peak_nb = sum(group_nb)
  ), .(sex, group_name)][, `:=`(
    group_peak_prc = group_peak_nb / sum(group_peak_nb)
  ), .(sex)][group_peak_prc >= min_group_peak_prc]

  peak_data <- data[peak_groups[, .(sex, group_name)], ,on = .(sex, group_name)]
  peak_data <- peak_data[!is.na(group_prc_year_ma)]
  setorder(peak_data, -group_prc_year_ma)
  peak_data <- peak_data[, .SD[1:top_years, year], .(sex, group_name)]
  peak_groups <- unique(peak_data[V1 >= year_begin & V1 <= year_end, .(sex, group_name)])

  data[peak_groups, , on = .(sex, group_name)]

}

filter_no_accent <- function(data) {

  groups_accent <- c(
    "ADELE", "AMELIA", "ANAIS", "ATHENA", "AURELIE", "BEATRICE", "CELESTE", "CLEMENCE", "ELEONORE",
    "ELODIE", "ELOISE", "EMILIE", "EVA", "EVE", "EVELYNE", "LEA ROSE", "LEANA", "LENA", "LEONIE",
    "MAEVA", "MAIKA", "NOELIE", "OPHELIA", "OPHELIE", "RAPHAELLE", "ZOE"
  )

  data[!(group_name %in% groups_accent)]

}

filter_namesake <- function(data, min_prc, max_nb) {

  data <- copy(data)

  keep_namesake_groups <- group_details(data)[, .(
    nb = sum(nb)
  ), .(sex, group_name, name)][, `:=`(
    group_name_nb = .N,
    group_nb = sum(nb),
    group_name_prc = nb / sum(nb)
  ), .(sex, group_name)][group_name == name & group_name_prc >= min_prc & group_name_nb <= max_nb]

  drop_namesake_groups <- group_details(data)[, .(
    nb = sum(nb)
  ), .(sex, group_name, name)][, `:=`(
    group_name_nb = .N,
    group_nb = sum(nb),
    group_name_prc = nb / sum(nb)
  ), .(sex, group_name)][group_name == name & !(group_name_prc >= min_prc & group_name_nb <= max_nb)]

  data[, drop := FALSE]
  data[drop_namesake_groups, drop := TRUE, on = .(sex, group_name)]
  data_filter <- data[drop == FALSE][, drop := NULL]

  data_filter[]

}
