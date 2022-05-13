# Loading packages ----------------------------------------------------------------------------

library(data.table)
library(stringi)
library(stringr)
library(ggplot2)

# Loading raw data ----------------------------------------------------------------------------

qc_h <- fread("data/qc-h-1980-2021.csv")
qc_f <- fread("data/qc-f-1980-2021.csv")

fr <- fread("data/fr-2020.csv")

# Cleaning data -------------------------------------------------------------------------------

data <- rbindlist(c(
  mapply(
    FUN = function(data, sex) {

      data[, `:=`(
        sex = sex,
        place = "Quebec"
      )]

      data <- melt(
        data = data[`Prénom/Année` != "Somme:" | is.na(`Prénom/Année`)],
        id.vars = c("sex", "place", "Prénom/Année"),
        measure.vars = as.character(1980:2021),
        variable.name = "year",
        value.name = "nb",
        variable.factor = FALSE
      )

      setnames(data, "Prénom/Année", "name")

      data[]

    },
    data = list(qc_h, qc_f),
    sex = c("M", "F"),
    SIMPLIFY = FALSE
  ), {

    fr[, place := "France"]

    fr[sexe == 1, sex := "M"]
    fr[sexe == 2, sex := "F"]
    fr[, sexe := NULL]

    fr[preusuel == "_PRENOMS_RARES", preusuel := NA]

    fr[annais == "XXXX", annais := NA]

    setnames(fr, c("preusuel", "annais", "nombre"), c("name", "year", "nb"))

    list(fr)

  }
), fill = TRUE)

# Removing raw data
rm(fr, qc_h, qc_f)

# Removing accents
data[, name := stri_trans_general(name, "upper; latin-ascii")]

# Removing hyphens
data[, name := gsub("-", " ", name)]

# Setting classes
data[, `:=`(
  sex = factor(sex, c("M", "F")),
  place = factor(place, c("Quebec", "France")),
  year = as.integer(year)
)]

# Combining duplicates
data <- data[, .(nb = sum(nb)), .(sex, place, name, year)]

# Detect multiple name
data[, multiple_name := str_detect(name, fixed(" "))]

# Calculating statistics
data[, `:=`(
  prc_sex_place_year = nb / sum(nb),
  rnk_sex_place_year = rank(-nb, ties.method = "min")
), .(sex, place, year)]

# Validation
# > data[, .(nb = sum(nb)), .(sex, place)]
# sex  place       nb
# 1:   M Quebec  2030260
# 2:   F Quebec  1928356
# 3:   M France 43638405
# 4:   F France 42967200

# Calculating summaries -----------------------------------------------------------------------

summary_sex_place <- data[year >= 1980 & !is.na(name), .(
  nb = sum(nb),
  min_prc_sex_place_year = min(prc_sex_place_year),
  max_prc_sex_place_year = max(prc_sex_place_year),
  range_prc_sex_place_year = max(prc_sex_place_year) - min(prc_sex_place_year),
  mean_prc_sex_place_year = mean(prc_sex_place_year),
  var_prc_sex_place_year = var(prc_sex_place_year),
  cv_prc_sex_place_year = sd(prc_sex_place_year) / mean(prc_sex_place_year)
), .(sex, place, name)][, `:=`(
  prc_sex_place = nb / sum(nb)
), .(sex, place)]

# Visualising ---------------------------------------------------------------------------------

theme_set(
  theme_minimal()
)

# Multiple name trend
ggplot(
  data = data[place == "Quebec" & !is.na(name), .(
    nb = sum(nb)
  ), .(sex, year, multiple_name)][, .(
    prc_multiple_name = .SD[multiple_name == TRUE, nb] / sum(nb)
  ), .(sex, year)],
  mapping = aes(
    x = year,
    y = prc_multiple_name,
    color = sex
  )
) +
  geom_line()

# Quebec vs France
ggplot(
  data = data[year >= 1980][summary_sex_place[, {
    .SD[order(-nb)][1:10]
  }, .(sex, place)], on = .(sex, place, name)],
  mapping = aes(
    x = year,
    y = prc_sex_place_year,
    group = name
  )
) +
  geom_line() +
  geom_line(
    data = function(x) x[x[, .N, .(sex, name)][N == max(N)], on = .(sex, name)],
    mapping = aes(
      color = name
    )
  ) +
  facet_grid(
    rows = vars(place),
    cols = vars(sex)
  ) +
  coord_cartesian(
    expand = FALSE
  )
