get_baseline <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")){

  if (section == "on-site" & module == "area") {
    sheet_name <- "A-1 On-Site Habitat Baseline"
    range <- "D9:X258"
    col_names <- LETTERS[4:24]
  } else if (section == "on-site" & module == "hedgerow") {
    sheet_name <- "B-1 On-Site Hedge Baseline"
    range <- "B8:X257"
    col_names <- LETTERS[2:24]
  } else if (section == "on-site" & module == "watercourse") {
    sheet_name <- "C-1 On-Site WaterC' Baseline"
    range <- "C8:Z257"
    col_names <- LETTERS[3:26]
  } else if (section == "off-site" & module == "area") {
    sheet_name <- "D-1 Off-Site Habitat Baseline"
    range <- "D9:AA258"
    col_names <- c(LETTERS[4:26], "AA")
  } else if (section == "off-site" & module == "hedgerow") {
    sheet_name <- "E-1 Off-Site Hedge Baseline"
    range <- "B8:X257"
    col_names <- LETTERS[2:24]
  } else if (section == "off-site" & module == "watercourse") {
    sheet_name <- "F-1 Off-Site WaterC' Baseline"
    range <- "C8:AC257"
    col_names <- c(LETTERS[3:26], "AA", "AB", "AC")
  } else {
    stop("Invalid section or module")
  }

  baseline <- readxl::read_excel(filepath,
                                 sheet = sheet_name,
                                 range = range,
                                 col_names = col_names) %>%
    as.data.frame() %>%
    dplyr::select(which(!is.na(.[2,])))

  names(baseline) <- baseline[2, ] %>%
    as.character() %>%
    snakecase::to_snake_case() %>%
    make.unique()


  baseline <- baseline %>%
    dplyr::slice(-c(1, 2))

  #get rid of the habitat type column for area module
  if (module == "area") {
    baseline <- baseline %>%
      dplyr::select(-habitat_type)
  }

    #rename columns so important ones are same for all modules
  baseline <- baseline %>%
    dplyr::rename_with(~ case_when(
      #off-site habitat baseline
      .x %in% "ref" ~ "parcel_ref",
      .x %in% c("broad_habitat", "habitat_type", "watercourse_type") ~ "habitat_name",
      .x %in% c("area_hectares", "length_km") ~ "baseline_size",
      .x %in% c("total_habitat_units", "total_hedgerow_units", "total_watercourse_units") ~ "baseline_units",
      .x %in% c("area_enhanced", "length_enhanced") ~ "baseline_enhancement_size",
      .x %in% c("baseline_units_enhanced", "units_enhanced") ~ "baseline_enhancement_units",
      .x %in% c("area_habitat_lost", "area_lost", "length_lost") ~ "lost_size",
      .x == "units_lost" ~ "lost_units",

      .default = .x
    )) %>%
    dplyr::select(parcel_ref, habitat_name, baseline_size, baseline_units, baseline_enhancement_size, baseline_enhancement_units, lost_size, lost_units) %>%
    dplyr::filter(!is.na(habitat_name))


  return(baseline)
}
