#' Implement validations for the auxiliary GDP table
#'
#' @param x data.frame: GDP table
#' @param schema data.frame: Expected schema for GDP table
#' @param max_year_to_year_change numeric: Maximum expected value for year-to-year
#' change in GDP
#' @param distinct_rows character: Combination of columns for which there should
#' be no duplicated rows
#'
#' @return pointblank object
#' @export
chk_out_gdp <- function(x,
                        schema,
                        max_year_to_year_change = 1.9,
                        distinct_rows = c('country_code',
                                          'data_level',
                                          'year')) {
  check_table_schema(x = x,
                     x_schema = schema) %>%
    pointblank::rows_distinct(columns = c(distinct_rows),
                              actions = pointblank::warn_on_fail(warn_at = 1)) %>%
    # Check that there is no crazy year to year change in GDP value
    pointblank::col_vals_lt(columns = "pct_change",
                            value = max_year_to_year_change,
                            preconditions = . %>%
                              dplyr::mutate(
                                year = as.numeric(year)
                              ) %>%
                              dplyr::group_by(country_code, data_level) %>%
                              dplyr::arrange(year, .by_group = TRUE) %>%
                              dplyr::mutate(
                                lead_value = dplyr::lead(value),
                              ) %>%
                              dplyr::ungroup() %>%
                              dplyr::mutate(
                                pct_change1 = abs((value / lead_value) - 1),
                                pct_change2 = abs((lead_value / value) - 1),
                                pct_change = ifelse(pct_change1 > pct_change2, pct_change1, pct_change2)
                              ),
                            actions = pointblank::action_levels(
                                warn_at = 1,
                              ),
                            na_pass = TRUE
    )
}




# agent <-
#   create_agent(
#     tbl = small_table,
#     tbl_name = "small_table",
#     label = "VALID-I Example No. 1"
#   ) %>%
#   col_is_posix(vars(date_time)) %>%
#   col_vals_in_set(vars(f), set = c("low", "mid", "high")) %>%
#   col_vals_lt(vars(a), value = 10) %>%
#   col_vals_regex(vars(b), regex = "^[0-9]-[a-z]{3}-[0-9]{3}$") %>%
#   col_vals_between(vars(d), left = 0, right = 5000) %>%
#   interrogate()
