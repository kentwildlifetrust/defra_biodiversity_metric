get_headline_results <- function(filepath){

  # Read in the headline results
  headline_results <- readxl::read_excel(filepath,
                                         sheet = "Headline Results",
                                         range = "B8:H30",
                                         col_names = c(letters[1:7])) %>%
    as.data.frame() %>%
    dplyr::select("figure_type" = 1,
                  "module" = 5,
                  "units" = 7) %>%
    dplyr::filter(!is.na(module)) %>%
    dplyr::mutate(units = replace(units, units %in% c(NA, "N/A"), 0)) %>%
    dplyr::mutate(units = round(as.numeric(units), 4)) %>%
    dplyr::mutate(figure_type = lapply(.$figure_type[!is.na(.$figure_type)],
                                       FUN = rep,
                                       times = 3) %>%
                    unlist() %>%
                    unname()) %>%
    dplyr::filter(!grepl("Off-site unit change", figure_type)) %>%
    dplyr::mutate(module = case_when(
                    module == "Habitat units" ~ "area",
                    module == "Hedgerow units" ~ "hedgerow",
                    module == "Watercourse units" ~ "watercourse",
                    .default = NA
                  ),
                  section = case_when(
                    grepl("On-site", figure_type) ~ "on-site",
                    grepl("Off-site", figure_type) ~ "off-site",
#                    grepl("Combined net", figure_type) ~ "combined",
#                    grepl("Spatial risk multiplier", figure_type) ~ "combined",
                    .default = NA
                  ),
                  type = case_when(
                    grepl("baseline", figure_type) ~ "base",
                    grepl("post-intervention", figure_type) ~ "post",
                    grepl("net change", figure_type) ~ "net",
                    grepl("net unit change", figure_type) ~ "net",
                    grepl("Spatial risk multiplier", figure_type) ~ "spatial deduction",
                    .default = NA
                  )) %>%
    dplyr::select(section,
                  module,
                  type,
                  units) %>%
    dplyr::arrange(section,
                   module,
                   type) %>%
    dplyr::filter(section != "combined")

  if (any(is.na(headline_results))) {
    stop("Error interpreting Headline Result sheet.")
  }

  return(headline_results)
}
