#' Get values from spreadsheet using sheet range
#'
#' @param sheet_range the range of the sheet to get values from. Must include a sheet name and a range.
#' @param n_merges the number of merged cells above a the sheet_range.
#'
#' @return
#' @export
#'
#' @examples
get_vals <- function(sheet_range, n_merges = 0){
  #get the sheet
  sheet_name <- strsplit(sheet_range, "!")[[1]][1] %>%
    gsub("'", "", .)
  sheet <- metric_sheets[[sheet_name]]
  stopifnot(!is.null(sheet))

  #get the range
  range <- strsplit(sheet_range, "!")[[1]][2] %>%
    gsub("\\$", "", .)
  range <- strsplit(range, ":")[[1]]

  #translate the range into data frame indexes
  col_start <- gsub("[0-9]", "", range[1]) %>%
    strsplit("")
  col_start <- match(col_start[[1]], LETTERS)
  col_start <- sapply(
    1:length(col_start),
    function(i) 26^(length(col_start) - i) * col_start[i]
  ) %>%
    sum()

  col_end <- gsub("[0-9]", "", range[2]) %>%
    strsplit("")
  col_end <- match(col_end[[1]], LETTERS)
  col_end <- sapply(
    1:length(col_end),
    function(i) 26^(length(col_end) - i) * col_end[i]
  ) %>%
    sum()

  row_start <- as.numeric(gsub("[A-Z]", "", range[1])) - 1 - n_merges #subtract one because header row
  row_end <- as.numeric(gsub("[A-Z]", "", range[2])) - 1 - n_merges

  #get the values
  vals <- sheet[row_start:row_end, col_start:col_end]
  return(vals)
}
