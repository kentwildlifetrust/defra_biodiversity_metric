get_detailed_results <- function(filepath){

  detailed_results <- readxl::read_excel(filepath,
                                         sheet = "Detailed Results",
                                         range = "B36:H207",
                                         col_names = c(LETTERS[1:7])) %>%
    as.data.frame() %>%
    dplyr::rename(habitat_name = A,
                  baseline_size = B,
                  baseline_units = C,
                  post_size = D,
                  post_units = E,
                  net_size = F,
                  net_units = G) %>%
    dplyr::filter(!is.na(net_units)) %>%
    dplyr::mutate(section = case_when(
                    grepl("On-site", baseline_size) ~ "on-site",
                    grepl("Off-site", baseline_size) ~ "off-site",
                    grepl("Combined", baseline_size) ~ "combined",
                    .default = NA
                  ),
                  module = case_when(
                    habitat_name == "Habitat group" ~ "area",
                    habitat_name == "Hedgerow type" ~ "hedgerow",
                    habitat_name == "Watercourse type" ~ "watercourse",
                    .default = NA
                  )) %>%
    tidyr::fill(section, module) %>%
    dplyr::filter(!(habitat_name %in% c("Habitat group", "Hedgerow type", "Watercourse type"))) %>%
    dplyr::mutate(
      habitat_name = stringr::str_squish(habitat_name),
      baseline_size = round(as.numeric(baseline_size), 4),
      baseline_units = round(as.numeric(baseline_units), 4),
      post_size = round(as.numeric(post_size), 4),
      post_units = round(as.numeric(post_units), 4),
      net_size = round(as.numeric(net_size), 4),
      net_units = round(as.numeric(net_units), 4)
    ) %>%
    dplyr::select(section, module, habitat_name, baseline_size, baseline_units, post_size, post_units, net_size, net_units) %>%
    dplyr::arrange(section, module, habitat_name)

  return(detailed_results)
}
