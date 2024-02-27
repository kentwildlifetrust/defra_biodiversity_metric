#' Util functions
#'
#' Functions to assist with extraction of data from spreadsheets, particularly renaming columns.
#' @export
#'
get_num_cols <- function(cell_range) {
  # Extract the column range from the cell range
  cols <- gsub("[0-9]", "", cell_range)

  # Split the column range into start and end columns
  cols_split <- strsplit(cols, ":")[[1]]

  # Extract the column indices
  start_col <- cols_split[1]
  end_col <- ifelse(length(cols_split) == 2, cols_split[2], cols_split[1])

  # Convert column letters to numeric indices
  start_col_index <- col2num(start_col)
  end_col_index <- col2num(end_col)

  # Calculate the number of columns
  num_cols <- abs(end_col_index - start_col_index) + 1

  return(num_cols)
}

# Helper function to convert column letters to numeric indices
col2num <- function(col) {
  letters <- LETTERS
  col <- toupper(col)
  num <- 0
  for (i in 1:nchar(col)) {
    num <- num * 26 + match(substr(col, i, i), letters)
  }
  return(num)
}

letter_cols <- function(num_cols) {
  i <- num_cols
  cols <- character(0)
  j <- 0
  new_letters <- LETTERS
  while (i > 0) {
    if (i < 26) {
      cols <- c(cols, new_letters[1:i])
      i <- 0
    } else {
      cols <- c(cols, new_letters[1:26])
      i <- i - 26
    }
    j <- j + 1
    new_letters <- paste0(LETTERS[j], LETTERS)
  }
  return(cols)
}
