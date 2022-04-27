#' Implement validations for the prod_svy_estimation table
#'
#' @param x data.frame: prod_svy_estimation table
#' @param schema data.frame: Expected schema for prod_svy_estimation table
#' @param distinct_rows character: Combination of columns for which there should
#' be no duplicated rows
#'
#' @return pointblank object
#' @export
chk_out_prod_svy_estimation <- function(x,
                                        schema,
                                        distinct_rows = c('country_code',
                                                          'reporting_year',
                                                          'welfare_type',
                                                          'reporting_level')) {
  check_table_schema(x = x,
                     x_schema = schema) |>
    pointblank::rows_distinct(columns = distinct_rows,
                              actions = pointblank::warn_on_fail(warn_at = 1))
}

