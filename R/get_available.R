#' Extract available units
#'
#' Extracts the available units for each broad area habitat, or hedgerow or watercourse type.
#'
#' @param filepath The path to the metric spreadsheet.
#' @param section The section of the metric spreadsheet to extract data from. Can be either "off-site" or "on-site".
#' @param module The module of the metric spreadsheet to extract data from. Can be either "area", "hedgerow" or "watercourse".
#' @return A data table including habitat_name, available_units and other relevant columns.
#'
#' @details
#' Available units are not a direct output from the metric spreadsheet, but are a useful way to provide prospective buyers with figures for individual broad area habitat, hedgerow or watercourse types.
#' Using net units in this way can be misleading, as net losses in other habitat types in the metric offsite provider's are not accounted for, even though these will need to be entered into the buyer's metric along with habitats getting a net gain.
#'
#' This function defines and calculates available units so they can provide buyers with (in most cases) a non-negative value for each post-intervention habitat type, taking into account losses in other habitat types.
#' Certain valid metric spreadsheets can produce negative available units, in which case a warning message is given.
#'
#' The starting premise is that available \eqn{A}{A} units are the net change in units in a given area of land.
#' Available units for each broad area habitat, watercourse or hedgerow type \eqn{i}{i} are therefore the result of subtracting the baseline units \eqn{B}{B} from the post units \eqn{P}{P} in the area occupied by habitat type at the end of the project.
#'
#' \deqn{A_{i} = P_{i} - B_{i}}{A_i = P_i - B_i}
#'
#' This is not the same as the net units given in the detailed results part of the metric spreadsheet, which are the result of subtracting the all the baseline units of that habitat type from the post units.
#'
#' To get the baseline and post figures, the Baseline, Enhancement and Creation data are extracted from the metric spreadsheet using the \code{get_baseline}, \code{get_enhancement} and \code{get_creation} functions. Areas of a given habitat that underwent enhancement or creation are considered separately:
#'
#' \deqn{A_{i} = A_{ei} + A_{ci}}{A_i = A_ei + A_ci}
#'
#' Where
#' \deqn{A_{ei} = P_{ei} - B_{ei}}{A_ei = P_ei - B_ei}
#' \deqn{A_{ci} = P_{ci} - B_{ci}}{A_ci = P_ci - P_ci}
#'
#' Baseline and post enhancement units can be identified for each habitat type by joining the baseline and enhancement data using the automatically generated Parcel Refs.
#'
#' Post creation units are listed in the Creation sheet, but the corresponding baseline creation units are not always provided. Here, they are estimated using the total baseline creation units or units lost across all \eqn{n} created habitat types in the selected module. (\eqn{\sum_{i=1}^{n}{B_{ci}}}), as well as the size of the habitat created (ha or km) \eqn{S_{i}}.
#'
#' \deqn{B_{ci} = P_{ci} - \frac{S_{ci}}{\sum_{i=1}^{n}{S_{ci}}} \times (\sum_{i=1}^{n}{P_{ci}} - \sum_{i=1}^{n}{B_{ci}})} {}
#'
#' Substituting into the equation for available units:
#' \deqn{A_{ci} = P_{ci} - (P_{ci} - \frac{S_{ci}}{\sum_{i=1}^{n}{S_{ci}}} \times (\sum_{i=1}^{n}{P_{ci}} - \sum_{i=1}^{n}{B_{ci}}))}{}
#' \deqn{A_{ci} = \frac{S_{ci}}{\sum_{i=1}^{n}{S_{ci}}} \times (\sum_{i=1}^{n}{P_{ci}} - \sum_{i=1}^{n}{B_{ci}}))}{}
#'
#' It is possible for the overall available units for a given habitat type to be be negative, if there are insufficient available enhancement units to cancel out a negative value of available creation units. In this case a result is still given, along with a warning message.
#' In these cases it is recommended that available units are determined manually by the metric provider.
#'
#' @export
get_available <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")) {

  baseline <- get_baseline(filepath, section, module) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  #get baseline and post enhancement units for each habitat type
  enhancement <- get_enhancement(filepath, section, module) %>%
    dplyr::left_join(baseline,dplyr::join_by("parcel_ref")) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(
      enhancement_size = sum(enhancement_size),
      post_enhancement_units = sum(post_enhancement_units),
      baseline_enhancement_units = sum(baseline_enhancement_units)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(available_enhancement_units = post_enhancement_units - baseline_enhancement_units)

  #get post creation units and size for each habitat type
  creation <- get_creation(filepath, section, module) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(post_creation_units = sum(post_creation_units),
                     creation_size = sum(creation_size)) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(baseline %>%
                     dplyr::group_by(baseline_habitat_name) %>%
                     dplyr::summarise(lost_units = sum(lost_units)),
                     dplyr::join_by(post_habitat_name == baseline_habitat_name)) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0)) %>%
    dplyr::mutate(available_creation_units = (creation_size / sum(.$creation_size)) * (sum(.$post_creation_units) - sum(.$lost_units)))


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
    dplyr::mutate(subtracted_units = net_units - available_units) %>%
    dplyr::mutate_if(is.numeric, function(x) round(x, 4)) %>%
    dplyr::select(habitat_name, baseline_units, post_units, post_enhancement_units,
                  baseline_enhancement_units, available_enhancement_units, creation_size,
                  post_creation_units, lost_units, available_creation_units, subtracted_units,
                  available_units, net_units)

  if (any(results$available_units < 0, na.rm = T)) {
    warning("Some available units are negative. This is possible when the total units delivered from creation is less than the total units lost.")
  }

  if (round(sum(results$net_units), 2) != round(sum(results$available_units), 2)) {
    stop("The total available units do not match the total net units. Please check the metric.")
  }

  return(results)
}

