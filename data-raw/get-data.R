library(data.table)

url_m <- "https://www.donneesquebec.ca/recherche/dataset/93d640ec-d059-4768-b7ed-388604b278aa/resource/039539f5-af55-4d8f-9010-ca718e45c2a5/download/gars_1980-2024.csv"
url_f <- "https://www.donneesquebec.ca/recherche/dataset/13db2583-427a-4e5f-b679-8532d3df571f/resource/bf77b504-54b9-4db8-be53-b92156175c12/download/filles_1980-2024.csv"

read_data_raw <- function(url) {

  fread(url, header = TRUE)

}

transform_raw_to_long <- function(sex, data_raw) {

  data_long <- copy(data_raw)
  data_long[, sex := sex]

  data_long <- melt(
    data = data_long[`Prénom/Année` != "Somme:" | is.na(`Prénom/Année`)],
    id.vars = c("sex", "Prénom/Année"),
    measure.vars = as.character(1980:2024),
    variable.name = "year",
    value.name = "nb",
    variable.factor = FALSE
  )

  setnames(data_long, "Prénom/Année", "name")

  data_long[]

}

transform_tidy <- function(data) {

  rbindlist(data[, data_long])

}

transform_clean <- function(data) {

  data <- copy(data)

  data[, year := as.integer(year)]
  data[nb == "< 5", nb := "0"]
  data[, nb := as.integer(nb)]

  data[, name := gsub("-", " ", name)]

  data[sex == "M" & name == "J SEBASTIEN", name := "JEAN SEBASTIEN"]
  data[sex == "F" & name == "M CHRISTINE", name := "MARIE CHRISTINE"]
  data[sex == "M" & name == "J FRANCOIS", name := "JEAN FRANCOIS"]
  data[sex == "M" & name == "J PHILIPPE", name := "JEAN PHILIPPE"]
  data[sex == "M" & name == "P OLIVIER", name := "P OLIVIER"]
  data[sex == "M" & name == "L PHILIPPE", name := "LOUIS PHILIPPE"]
  data[sex == "M" & name == "J CHRISTOPHE", name := "JEAN CHRISTOPHE"]
  data[sex == "F" & name == "M MICHELLE", name := "MARIE MICHELLE"]
  data[sex == "M" & name == "P ALEXANDRE", name := "PIERRE ALEXANDRE"]
  data[sex == "M" & name == "M ALEXANDRE", name := "MARC ALEXANDRE"]
  data[sex == "F" & name == "M LAURENCE", name := "MARIE LAURENCE"]
  data[sex == "M" & name == "P ETIENNE", name := "PIERRE ETIENNE"]
  data[sex == "M" & name == "F XAVIER", name := "FRANCOIS XAVIER"]
  data[sex == "M" & name == "C OLIVIER", name := "CHARLES OLIVIER"]
  data[sex == "M" & name == "C ETIENNE", name := "CHARLES ETIENNE"]
  data[sex == "F" & name == "M CHANTALE", name := "MARIE CHANTALE"]
  data[sex == "M" & name == "C ANTOINE", name := "CHARLES ANTOINE"]
  data[sex == "F" & name == "M MICHELE", name := "MARIE MICHELE"]
  data[sex == "M" & name == "M PHILIPPE", name := "MARC PHILIPPE"]
  data[sex == "F" & name == "M HELENE", name := "MARIE HELENE"]
  data[sex == "F" & name == "M CATHERINE", name := "MARIE CATHERINE"]
  data[sex == "M" & name == "P ANTOINE", name := "P ANTOINE"]
  data[sex == "M" & name == "P PHILIPPE", name := "PIERRE PHILIPPE"]
  data[sex == "M" & name == "J ALEXANDRE", name := "JEAN ALEXANDRE"]
  data[sex == "M" & name == "C ALEXANDRE", name := "CHARLES ALEXANDRE"]
  data[sex == "M" & name == "F OLIVIER", name := "F OLIVIER"]
  data[sex == "M" & name == "M OLIVIER", name := "MARC OLIVIER"]
  data[sex == "M" & name == "A PHILIPPE", name := "ANDRE PHILIPPE"]
  data[sex == "F" & name == "M PHILIPPE", name := "MARIE PHILIPPE"]
  data[sex == "M" & name == "D ALEXANDRE", name := "DAVID ALEXANDRE"]
  data[sex == "M" & name == "L ALEXANDRE", name := "LOUIS ALEXANDRE"]
  data[sex == "F" & name == "M CLAUDE", name := "MARIE CLAUDE"]

  data[, total_nb := sum(nb), .(sex, name)]

  data[total_nb > 0L, .(nb = sum(nb)), .(sex, name, year)]

}

data <- data.table(
  sex = factor(c("M", "F")),
  url = c(url_m, url_f)
)

data[, data_raw := lapply(url, read_data_raw)]

data[, data_long := mapply(
  FUN = transform_raw_to_long,
  sex = sex,
  data_raw = data_raw,
  SIMPLIFY = FALSE
)] |>
  transform_tidy() |>
  transform_clean() ->
  data

# TODO: Grouping namesakes

fwrite(
  x = data,
  file = "data/data-clean.csv"
)
