get_baseline <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")){

  baseline_lookup <- data.frame(
    section = rep(c("on-site", "off-site"), each = 3),
    module = rep(c("area", "hedgerow", "watercourse"), 2),
    sheet_name = c("A-1 On-Site Habitat Baseline",
                   "B-1 On-Site Hedge Baseline",
                   "C-1 On-Site WaterC' Baseline",
                   "D-1 Off-Site Habitat Baseline",
                   "E-1 Off-Site Hedge Baseline",
                   "F-1 Off-Site WaterC' Baseline"),
    range = c("D9:X258", "B8:X257", "C8:Z257", "D9:AA258", "B8:X257", "C8:AC257")
  )

  selected <- which(baseline_lookup$section == section & baseline_lookup$module == module)

  if (length(selected) == 0) {
    stop("Invalid section or module")
  }

  baseline <- readxl::read_excel(filepath,
                                 sheet = baseline_lookup$sheet_name[selected],
                                 range = baseline_lookup$range[selected],
                                 col_names = letter_cols(get_num_cols(baseline_lookup$range[selected]))) %>%
    as.data.frame() %>%
    dplyr::select(which(!is.na(.[2, ])))

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
      .x %in% c("broad_habitat", "habitat_type", "watercourse_type") ~ "baseline_habitat_name",
      .x %in% c("area_hectares", "length_km") ~ "baseline_size",
      .x %in% c("total_habitat_units", "total_hedgerow_units", "total_watercourse_units") ~ "baseline_units",
      .x %in% c("area_enhanced", "length_enhanced") ~ "enhancement_size",
      .x %in% c("baseline_units_enhanced", "units_enhanced") ~ "baseline_enhancement_units",
      .x %in% c("area_habitat_lost", "area_lost", "length_lost") ~ "lost_size",
      .x == "units_lost" ~ "lost_units",
      .default = .x
    )) %>%
    dplyr::select(parcel_ref, baseline_habitat_name, baseline_size, baseline_units, enhancement_size, baseline_enhancement_units, lost_size, lost_units) %>%
    dplyr::mutate(
      baseline_size = round(as.numeric(baseline_size), 4),
      baseline_units = round(as.numeric(baseline_units), 4),
      enhancement_size = round(as.numeric(enhancement_size), 4),
      baseline_enhancement_units = round(as.numeric(baseline_enhancement_units), 4),
      lost_size = round(as.numeric(lost_size), 4),
      lost_units = round(as.numeric(lost_units), 4)
    ) %>%
    dplyr::filter(!is.na(baseline_habitat_name))


  return(baseline)
}
