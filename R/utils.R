#' Validate table schema
#'
#' @param x data.frame: Table to be validated
#' @param x_schema data.frame: Schema the x table must be validated against
#'
#' @return list
#' @export
#'
#' @examples
#' x <- data.frame(
#' var1 = c("a", "b"),
#' var2 = c(1, 2))
#' x_schema <- data.frame(
#' var1 = character(0),
#' var2 = numeric(0))
#' check_table_schema(x = x, x_schema = x_schema)

check_table_schema <- function(x,
                               x_schema) {
  cols_chr <- names(x_schema[unlist(lapply(x_schema, is.character))])
  cols_num <- names(x_schema[unlist(lapply(x_schema, is.numeric))])
  cols_lgc <- names(x_schema[unlist(lapply(x_schema, is.logical))])

  # Validate columns names
  pointblank::col_exists(x = x,
                         columns = names(x_schema),
                         label = "Structural check: Column names",
                         actions = pointblank::warn_on_fail(warn_at = 1)) %>%
    # Validate columns types - Character
    { if (length(cols_chr > 0)) {
      pointblank::col_is_character(x = .,
                                   columns = tidyselect::all_of(cols_chr),
                                   label = "Structural check: Column types - character",
                                   actions = pointblank::warn_on_fail(warn_at = 1))
    } else {
      . }
    } %>%
    # Validate columns types - Numeric
    { if (length(cols_num > 0)) {
      pointblank::col_is_numeric(x = .,
                                 columns = tidyselect::all_of(cols_num),
                                 label = "Structural check: Column types - numeric",
                                 actions = pointblank::warn_on_fail(warn_at = 1))
    } else {
      . }
    } %>%
    # Validate columns types - Logical
    { if (length(cols_lgc > 0)) {
      pointblank::col_is_logical(x = .,
                                 columns = tidyselect::all_of(cols_lgc),
                                 label = "Structural check: Column types - logical",
                                 actions = pointblank::warn_on_fail(warn_at = 1))
    } else {
      . }
    }

}
