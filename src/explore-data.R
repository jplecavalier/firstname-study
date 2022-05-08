# Loading packages ----------------------------------------------------------------------------

library(data.table)
library(stringr)
library(ggplot2)

# Loading raw data ----------------------------------------------------------------------------

qc_h <- fread("data/qc-h-1980-2020.csv")
qc_f <- fread("data/qc-f-1980-2020.csv")

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
        measure.vars = as.character(1980:2020),
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

# Setting classes
data[, `:=`(
  sex = factor(sex, c("M", "F")),
  place = factor(place, c("Quebec", "France")),
  name = as.factor(name),
  year = as.integer(year)
)]

# Combining duplicates
data <- data[, .(nb = sum(nb)), .(sex, place, name, year)]

# Calculating statistics
data[, prc_sex_place_year := nb / sum(nb), .(sex, place, year)]

# Validation
# > data[, .(nb = sum(nb)), .(sex, place)]
# sex  place       nb
# 1:   M Quebec  1978661
# 2:   F Quebec  1879691
# 3:   M France 43638405
# 4:   F France 42967200

# Calculating summaries -----------------------------------------------------------------------

summary_sex_place <- data[year >= 1980 & !is.na(name), .(nb = sum(nb)), .(sex, place, name)]

# Visualising ---------------------------------------------------------------------------------

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
