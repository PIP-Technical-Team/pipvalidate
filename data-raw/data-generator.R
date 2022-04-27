
source("./data-raw/prod_svy_estimation_schema.R")
source("./data-raw/gdp_schema.R")

usethis::use_data(prod_svy_estimation_schema,
                  gdp_schema,
                  internal = TRUE,
                  overwrite = TRUE)



