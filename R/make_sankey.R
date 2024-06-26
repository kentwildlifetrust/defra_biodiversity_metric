#' Show habitat changes as Sankey Diagram
#'
#' @param filepath The path to the metric spreadsheet.
#' @param section The section of the metric spreadsheet to extract data from. Can be either "off-site" or "on-site".
#' @param module The module of the metric spreadsheet to extract data from. Can be either "area", "hedgerow" or "watercourse".
#' @return A Sankey Diagram showing habitat changes. Height of nodes represents area of habitat, colour represent density of BNG units.
#'
#' @return Sankey Diagram
#' @export
#'
#'
make_sankey <- function(filepath, section = c("off-site", "on-site"), module = c("area", "hedgerow", "watercourse")) {

  baseline <- get_baseline(filepath, section, module) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  enhancement <- get_enhancement(filepath, section, module) %>%
    dplyr::mutate_if(is.numeric, function(x) replace(x, is.na(x), 0))

  baseline_retained <- baseline %>%
    dplyr::group_by(baseline_habitat_name) %>%
    dplyr::mutate(retention_size = baseline_size - lost_size - enhancement_size,
                  retention_units = baseline_units - baseline_enhancement_units - lost_units) %>%
    dplyr::filter(retention_size > 0) %>%
    dplyr::summarise(retention_size = sum(retention_size),
                     retention_units = sum(retention_units)) %>%
    dplyr::mutate(retained_units_per_ha = round(retention_units / retention_size, 2)) %>%
    dplyr::mutate(post_habitat_name = baseline_habitat_name,
                  change_type = "Retained as") %>%
    dplyr::select(baseline_habitat_name,
                  baseline_units_per_ha = retained_units_per_ha,
                  baseline_units = retention_units,
                  size = retention_size,
                  post_habitat_name,
                  post_units = retention_units,
                  post_units_per_ha = retained_units_per_ha,
                  change_type)

  baseline_enhanced <- baseline %>%
    dplyr::filter(enhancement_size > 0) %>%
    dplyr::left_join(enhancement, dplyr::join_by(parcel_ref == parcel_ref)) %>%
    dplyr::group_by(baseline_habitat_name, post_habitat_name) %>%
    dplyr::summarise(enhancement_size = sum(enhancement_size),
                     post_enhancement_units = sum(post_enhancement_units),
                     baseline_enhancement_units = sum(baseline_enhancement_units)) %>%
    dplyr::mutate(baseline_enhancement_units_per_ha = round(baseline_enhancement_units / enhancement_size, 2),
                  post_enhancement_units_per_ha = round(post_enhancement_units / enhancement_size, 2)) %>%
    dplyr::mutate(change_type = "Enhanced to") %>%
    dplyr::select(baseline_habitat_name,
                  baseline_units_per_ha = baseline_enhancement_units_per_ha,
                  baseline_units = baseline_enhancement_units,
                  size = enhancement_size,
                  post_habitat_name,
                  post_units = post_enhancement_units,
                  post_units_per_ha = post_enhancement_units_per_ha,
                  change_type)

  baseline_loss <- baseline %>%
    dplyr::filter(lost_units > 0) %>%
    dplyr::group_by(baseline_habitat_name) %>%
    dplyr::summarise(lost_size = sum(lost_size),
                     lost_units = sum(lost_units)) %>%
    dplyr::mutate(lost_units_per_ha = round(lost_units / lost_size, 2)) %>%
    dplyr::select(baseline_units_per_ha = lost_units_per_ha,
                  baseline_habitat_name,
                  baseline_units = lost_units,
                  size = lost_size) %>%
    dplyr::mutate(post_habitat_name = "Loss",
                  post_units_per_ha = NA,
                  post_units = baseline_units,
                  change_type = "Loss")

  creation <- get_creation(filepath, section, module) %>%
    dplyr::group_by(post_habitat_name) %>%
    dplyr::summarise(creation_size = sum(creation_size),
                     post_creation_units = sum(post_creation_units)) %>%
    dplyr::mutate(post_creation_units_per_ha = round(post_creation_units / creation_size, 2),
                  baseline_habitat_name = "Loss",
                  change_type = "Changed to") %>%
    dplyr::mutate(post_habitat_name = post_habitat_name,
                  baseline_units_per_ha = NA,
                  baseline_units = NA) %>%
    dplyr::select(baseline_habitat_name,
                  baseline_units_per_ha,
                  baseline_units,
                  size = creation_size,
                  post_habitat_name,
                  post_units_per_ha = post_creation_units_per_ha,
                  post_units = post_creation_units,
                  change_type)


  all_metric <- rbind(baseline_retained, baseline_enhanced, baseline_loss, creation) %>%
    as.data.frame() %>%
    dplyr::mutate(post_habitat_name_key = paste(post_units_per_ha, post_habitat_name),
                  baseline_habitat_name_key = paste(baseline_units_per_ha, baseline_habitat_name)) %>%
    dplyr::mutate(size = as.numeric(size)) %>%
    dplyr::filter(size > 0.009)

  total_units_baseline_lookup <- all_metric %>%
    dplyr::group_by(baseline_habitat_name_key) %>%
    dplyr::summarise(total_baseline_units = sum(baseline_units))

  total_units_post_lookup <- all_metric %>%
    dplyr::group_by(post_habitat_name_key) %>%
    dplyr::summarise(total_post_units = sum(post_units))

  total_lost_units <- round(total_units_post_lookup$total_post_units[grepl("Loss", total_units_post_lookup$post_habitat_name_key)], 2)

  # total_baseline_size_lookup <- all_metric %>%
  #   dplyr::group_by(baseline_habitat_name) %>%
  #   dplyr::summarise(total_baseline_units = sum(baseline_units))
  #

  all_metric <- all_metric %>%
    dplyr::left_join(total_units_baseline_lookup, dplyr::join_by(baseline_habitat_name_key == baseline_habitat_name_key)) %>%
    dplyr::left_join(total_units_post_lookup, dplyr::join_by(post_habitat_name_key == post_habitat_name_key)) %>%
    dplyr::mutate(baseline_habitat_name = paste(round(total_baseline_units, 2), "units of", baseline_habitat_name),
                  post_habitat_name = paste(change_type, round(total_post_units, 2), "units of", post_habitat_name))

  all_metric$baseline_habitat_name[grepl("Loss", all_metric$baseline_habitat_name)] <- paste(total_lost_units, "units lost")
  all_metric$post_habitat_name[grepl("Loss", all_metric$post_habitat_name)] <- paste(total_lost_units, "units lost")



  #https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html
  # A connection data frame is a list of flows with intensity for each flow
  links <- all_metric %>%
    dplyr::distinct() %>%
    dplyr::select(source = baseline_habitat_name,
                  target = post_habitat_name,
                  value = size)

  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes<- rbind(
    dplyr::select(all_metric,
                  name = baseline_habitat_name,
                  group = baseline_units_per_ha),
    dplyr::select(all_metric,
                  name = post_habitat_name,
                  group = post_units_per_ha)
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(group = ifelse(is.na(group), "Loss", group)) %>%
    dplyr::mutate(group = as.factor(group))

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  group_vals <- c(unique(as.numeric(levels(nodes$group))))
  palette <- scales::col_numeric(group_vals, palette = "viridis")
  range <- palette(group_vals)

  colour_scale <- paste0("d3.scaleOrdinal().domain([",
                        paste(paste0("'", c(group_vals, "Loss"), "'"), collapse = ", "),
                        "]).range([",
                        paste(paste0("'", c(range, "gray"), "'"), collapse = ", "),
                        "])")

  # Make the Network
  p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     colourScale = colour_scale,
                     NodeGroup = "group",
                     sinksRight=FALSE,
                     fontSize = 12)

  return(p)
}
