library(data.table)

source("src/utils.R")
source("src/visuals.R")

data <- fread("data/data-clean.csv")

data_group_details <- group_details(data)
data_summary <- summary_all_years(data)

options_name <- filter_gen(data, 2013L, 2028L, 250L)[sex == "F"]

options_name_summary <- data_summary[sex == "F" & group_name %in% options_name[, unique(group_name)]]
options_name_summary <- options_name_summary[!(group_name %in% drop_namesake_group(data, 0.6, 3)[sex == "F", group_name])]

View(options_name_summary[name == group_name, .(group_label, group_nb, group_cv_prc_year_ma)])

individual_group_facets(data, "F", options_name_summary[, sample(group_name, 4L, prob = group_nb)])
