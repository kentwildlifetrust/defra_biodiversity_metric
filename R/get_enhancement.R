#' Extract habitat enhancement details
#'
#' Extracts details from the habitat enhancement parts of the metric spreadsheet.
#' @param filepath The path to the metric spreadsheet.
#' @param section The section of the metric spreadsheet to extract data from. Can be either "off-site" or "on-site".
#' @param module The module of the metric spreadsheet to extract data from. Can be either "area", "hedgerow" or "watercourse".
#' @return A data table including parcel_ref, post_habitat_name, post_enhancement_units.
#' @export
#'
get_enhancement <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")){

  enhancement_lookup <- data.frame(
    section = rep(c("on-site", "off-site"), each = 3),
    module = rep(c("area", "hedgerow", "watercourse"), 2),
    sheet_name = c("A-3 On-Site Habitat Enhancement",
                   "B-3 On-Site Hedge Enhancement",
                   "C-3 On-Site WaterC' Enhancement",
                   "D-3 Off-Site Habitat Enhancment",
                   "E-3 Off-Site Hedge Enhancement",
                   "F-3 Off-Site WaterC Enhancement"),
    range = c("E10:AN257", "B10:AH257", "B10:AM257", "E10:AQ258", "B10:AK257", "B10:AP257")
  )


  selected <- which(enhancement_lookup$section == section & enhancement_lookup$module == module)

  if (length(selected) == 0) {
    stop("Invalid section or module")
  }

  enhancement <- readxl::read_excel(filepath,
                                 sheet = enhancement_lookup$sheet_name[selected],
                                 range = enhancement_lookup$range[selected],
                                 col_names = letter_cols(get_num_cols(enhancement_lookup$range[selected]))) %>%
    as.data.frame()


  enhancement[2, as.logical(is.na(enhancement[2, ]))] <- enhancement[1, as.logical(is.na(enhancement[2, ]))]

  names(enhancement) <- enhancement[2, ] %>%
    as.character() %>%
    snakecase::to_snake_case() %>%
    make.unique()

  #get rid of the first two rows (headers)
  enhancement <- enhancement %>%
    dplyr::slice(-c(1, 2))

  #get rid of the habitat type column for area module
  if (module == "area") {
    enhancement <- enhancement %>%
      dplyr::select(-proposed_habitat)
  }

  #rename columns so important ones are same for all modules
  enhancement <- enhancement %>%
    dplyr::rename_with(~ dplyr::case_when(
      #off-site habitat enhancement
      .x %in% "baseline_ref" ~ "parcel_ref",
      .x %in% c("proposed_broad_habitat", "proposed_habitat") ~ "post_habitat_name",
      .x %in% c("habitat_units_delivered", "hedge_units_delivered", "watercourse_units_delivered") ~ "post_enhancement_units",
      .default = .x
    )) %>%
    dplyr::select(parcel_ref, post_habitat_name, post_enhancement_units) %>%
    dplyr::mutate(post_enhancement_units = round(as.numeric(post_enhancement_units), 4)) %>%
    dplyr::filter(!is.na(post_habitat_name))


  return(enhancement)

}
