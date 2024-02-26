get_available <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")) {

  baseline <- get_baseline(filepath, section, module) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  #get baseline, enhancement and creation data
  enhancement <- get_enhancement(filepath, section, module) %>%
    dplyr::left_join(baseline,dplyr::join_by("parcel_ref")) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(
      enhancement_size = sum(enhancement_size),
      available_enhancement_units = sum(post_enhancement_units - baseline_enhancement_units)
    )


  creation <- get_creation(filepath, section, module) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(post_creation_units = sum(post_creation_units),
                     creation_size = sum(creation_size)) %>%
    dplyr::full_join(baseline %>%
                       dplyr::group_by(baseline_habitat_name) %>%
                       dplyr::summarise(lost_units = sum(lost_units)),
                     join_by(post_habitat_name == baseline_habitat_name)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0)) %>%
    dplyr::mutate(available_creation_units = post_creation_units -
                    (creation_size / sum(creation_size)) * sum(lost_units)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  #start with detailed results
  s <- section
  m <- module
  results <- get_detailed_results(filepath) %>%
    dplyr::filter(section == s &
                  module == m) %>%
    dplyr::left_join(enhancement, dplyr::join_by(habitat_name == post_habitat_name)) %>%
    dplyr::left_join(creation, dplyr::join_by(habitat_name == post_habitat_name)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0)) %>%
    dplyr::mutate(available_units = available_enhancement_units + available_creation_units) %>%
    dplyr::select(habitat_name, available_units)

  return(results)
}

