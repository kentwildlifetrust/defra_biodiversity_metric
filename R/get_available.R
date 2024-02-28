#' Extract available units
#'
#' Extracts the available units for each broad area habitat, or hedgerow or watercourse type.
#' @param filepath The path to the metric spreadsheet.
#' @param section The section of the metric spreadsheet to extract data from. Can be either "off-site" or "on-site".
#' @param module The module of the metric spreadsheet to extract data from. Can be either "area", "hedgerow" or "watercourse".
#' @return A data table including habitat_name, available_units and other relevant columns.
#' @export
#'
get_available <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")) {

  baseline <- get_baseline(filepath, section, module) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  #get baseline, enhancement and creation data
  enhancement <- get_enhancement(filepath, section, module) %>%
    dplyr::left_join(baseline,dplyr::join_by("parcel_ref")) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(
      enhancement_size = sum(enhancement_size),
      post_enhancement_units = sum(post_enhancement_units),
      baseline_enhancement_units = sum(baseline_enhancement_units),
      available_enhancement_units = sum(post_enhancement_units - baseline_enhancement_units)
    )


  creation <- get_creation(filepath, section, module) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(post_creation_units = sum(post_creation_units),
                     creation_size = sum(creation_size)) %>%
    dplyr::full_join(baseline %>%
                       dplyr::group_by(baseline_habitat_name) %>%
                       dplyr::summarise(lost_units = sum(lost_units)),
                     dplyr::join_by(post_habitat_name == baseline_habitat_name)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0)) %>%
    dplyr::mutate(available_creation_units = post_creation_units -
                    (creation_size / sum(creation_size)) * sum(lost_units)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  if (sum(creation$available_creation_units) < 0) {
    stop("The total available units from creation is less than 0. Please check the metric.")
  }

  creation <- creation %>%
    dplyr::mutate(available_creation_units = ifelse(available_creation_units < 0,
                                                    0,
                                                    available_creation_units))

  total_discrepancy <- sum(creation$available_creation_units -
                             (creation$post_creation_units - creation$lost_units))

  creation <- creation %>%
    dplyr::mutate(available_creation_units =
                    available_creation_units - total_discrepancy * available_creation_units / sum(available_creation_units)) %>%
    dplyr::mutate(subtracted_units = post_creation_units - available_creation_units) %>%
    dplyr::mutate(subtracted_units = ifelse(subtracted_units < 0, 0, subtracted_units))

  #start with detailed results
  s <- section
  m <- module
  results <- get_detailed_results(filepath) %>%
    dplyr::filter(section == s &
                  module == m) %>%
    dplyr::left_join(enhancement, dplyr::join_by(habitat_name == post_habitat_name)) %>%
    dplyr::left_join(creation, dplyr::join_by(habitat_name == post_habitat_name)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0)) %>%
    dplyr::mutate_if(is.numeric, function(x) round(x, 4)) %>%
    dplyr::mutate(available_units = available_enhancement_units + available_creation_units) %>%
    dplyr::select(habitat_name, baseline_units, post_units, post_enhancement_units,
                  baseline_enhancement_units, available_enhancement_units, creation_size,
                  post_creation_units, lost_units, available_creation_units, subtracted_units,
                  available_units, net_units)

  if (round(sum(results$net_units), 2) != round(sum(results$available_units), 2)) {
    stop("The total available units do not match the total net units. Please check the metric.")
  }

  return(results)
}

