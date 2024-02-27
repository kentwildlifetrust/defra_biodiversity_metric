#' Extract habitat creation details
#'
#' Extracts details from the habitat creation parts of the metric spreadsheet.
#' @param filepath The path to the metric spreadsheet.
#' @param section The section of the metric spreadsheet to extract data from. Can be either "off-site" or "on-site".
#' @param module The module of the metric spreadsheet to extract data from. Can be either "area", "hedgerow" or "watercourse".
#' @return A data table including post_habitat_name, creation_size and post_creation_units.
#' @export
#'
get_creation <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")){

  creation_lookup <- data.frame(
    section = rep(c("on-site", "off-site"), each = 3),
    module = rep(c("area", "hedgerow", "watercourse"), 2),
    sheet_name = c("A-2 On-Site Habitat Creation",
                   "B-2 On-Site Hedge Creation",
                   "C-2 On-Site WaterC' Creation",
                   "D-2 Off-Site Habitat Creation",
                   "E-2 Off-Site Hedge Creation",
                   "F-2 Off-Site WaterC' Creation"),
    range = c("D9:Y256", "C10:W259", "C10:Z259", "D9:AB256", "C10:Z259", "C10:AC259")
  )


  selected <- which(creation_lookup$section == section & creation_lookup$module == module)

  if (length(selected) == 0) {
    stop("Invalid section or module")
  }

  creation <- readxl::read_excel(filepath,
                                    sheet = creation_lookup$sheet_name[selected],
                                    range = creation_lookup$range[selected],
                                    col_names = letter_cols(get_num_cols(creation_lookup$range[selected]))) %>%
    as.data.frame()


  creation[2, as.logical(is.na(creation[2, ]))] <- creation[1, as.logical(is.na(creation[2, ]))]

  names(creation) <- creation[2, ] %>%
    as.character() %>%
    snakecase::to_snake_case() %>%
    make.unique()

  #get rid of the first two rows (headers)
  creation <- creation %>%
    dplyr::slice(-c(1, 2))

  #get rid of the habitat type column for area module
  if (module == "area") {
    creation <- creation %>%
      dplyr::select(-proposed_habitat)
  }

  #rename columns so important ones are same for all modules
  creation <- creation %>%
    dplyr::rename_with(~ case_when(
      #off-site habitat creation
      .x %in% c("broad_habitat", "habitat_type", "watercourse_type") ~ "post_habitat_name",
      .x %in% c("area_hectares", "length_km") ~ "creation_size",
      .x %in% c("habitat_units_delivered", "hedge_units_delivered", "watercourse_units_delivered") ~ "post_creation_units",
      .default = .x
    )) %>%
    dplyr::select(post_habitat_name, creation_size, post_creation_units) %>%
    dplyr::mutate(post_creation_units = round(as.numeric(post_creation_units), 4),
                  creation_size = round(as.numeric(creation_size), 4)) %>%
    dplyr::filter(!is.na(post_habitat_name))


  return(creation)

}
