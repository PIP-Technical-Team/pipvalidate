# GDP table is returned in a wide format by the API
# This is problematic because it means that the schema will change everytime
# a new year of GDP is being added
# To avoid that issue. The GDP table is turned into a long format table before
# validation with the follwong code:
# gdp <- tidyr::pivot_longer(gdp,
#                            cols = !tidyr::all_of(c("country_code", "data_level")),
#                            names_to = "year",
#                            values_to = "value")

gdp_schema <-
  data.frame(
    country_code = character(0),
    data_level = character(0),
    year = character(0),
    value = numeric(0)
  )
