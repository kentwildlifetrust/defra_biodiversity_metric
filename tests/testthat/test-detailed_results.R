test_that("headline results extract", {
  for (file in list.files(system.file("metric_examples", package = "DefraBiodiversityMetric"))) {
    file_path <- system.file("metric_examples", file, package = "DefraBiodiversityMetric")
    expect_equal(class(get_headline_results(file_path)), "data.frame")
  }
})


template_df <- data.frame(
  section = rep(c("combined", "off-site", "on-site"), each = 33) %>%
    as.vector(),
  module = rep(c(rep("area", 15),
               rep("hedgerow", 13),
               rep("watercourse", 5)), 3),
  habitat_name = rep(c(c("Cropland",
                       "Grassland",
                       "Heathland and shrub",
                       "Lakes",
                       "Sparsely vegetated land",
                       "Urban",
                       "Wetland",
                       "Woodland and forest",
                       "Intertidal sediment",
                       "Coastal saltmarsh",
                       "Rocky shore",
                       "Coastal lagoons",
                       "Intertidal hard structures",
                       "Watercourse footprint",
                       "Individual trees") %>% sort(),
                     c("Species-rich native hedgerow with trees - associated with bank or ditch",
                       "Species-rich native hedgerow with trees",
                       "Species-rich native hedgerow - associated with bank or ditch",
                       "Native hedgerow with trees - associated with bank or ditch",
                       "Species-rich native hedgerow",
                       "Native hedgerow - associated with bank or ditch",
                       "Native hedgerow with trees",
                       "Ecologically valuable line of trees",
                       "Ecologically valuable line of trees - associated with bank or ditch",
                       "Native hedgerow",
                       "Line of trees",
                       "Line of trees - associated with bank or ditch",
                       "Non-native and ornamental hedgerow") %>% sort(),
                     c("Priority habitat",
                       "Other rivers and streams",
                       "Ditches",
                       "Canals",
                       "Culvert") %>% sort()), times = 3),
  baseline_size = rep(0, 99),
  baseline_units = rep(0, 99),
  post_size = rep(0, 99),
  post_units = rep(0, 99),
  net_size = rep(0, 99),
  net_units = rep(0, 99)
)

test_that("detailed results get extracted correctly", {
  haffenden_farm <- template_df

  #off-site cropland
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "off-site" &
                         haffenden_farm$module == "area" &
                         haffenden_farm$habitat_name == "Cropland"] <- 3.426
  haffenden_farm$baseline_units[haffenden_farm$section == "off-site" &
                         haffenden_farm$module == "area" &
                           haffenden_farm$habitat_name == "Cropland"] <- 7.5372
  #change
  haffenden_farm$net_size[haffenden_farm$section == "off-site" &
                                 haffenden_farm$module == "area" &
                                 haffenden_farm$habitat_name == "Cropland"] <- -3.426
  haffenden_farm$net_units[haffenden_farm$section == "off-site" &
                                  haffenden_farm$module == "area" &
                                  haffenden_farm$habitat_name == "Cropland"] <- -7.5372

  #off-site grassland
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "off-site" &
                                 haffenden_farm$module == "area" &
                                 haffenden_farm$habitat_name == "Grassland"] <- 56.072
  haffenden_farm$baseline_units[haffenden_farm$section == "off-site" &
                                  haffenden_farm$module == "area" &
                                  haffenden_farm$habitat_name == "Grassland"] <- 803.1232
  #post
  haffenden_farm$post_size[haffenden_farm$section == "off-site" &
                           haffenden_farm$module == "area" &
                           haffenden_farm$habitat_name == "Grassland"] <- 59.498
  haffenden_farm$post_units[haffenden_farm$section == "off-site" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Grassland"] <- 1142.2133

  #change
  haffenden_farm$net_size[haffenden_farm$section == "off-site" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Grassland"] <- 3.426
  haffenden_farm$net_units[haffenden_farm$section == "off-site" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Grassland"] <- 339.0901

  #off-site heathland and shrub
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "off-site" &
                               haffenden_farm$module == "area" &
                               haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.096
  haffenden_farm$baseline_units[haffenden_farm$section == "off-site" &
                                haffenden_farm$module == "area" &
                                haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.8448
  #post
  haffenden_farm$post_size[haffenden_farm$section == "off-site" &
                           haffenden_farm$module == "area" &
                           haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.096
  haffenden_farm$post_units[haffenden_farm$section == "off-site" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Heathland and shrub"] <- 1.2244
  #change
  haffenden_farm$net_size[haffenden_farm$section == "off-site" &
                          haffenden_farm$module == "area" &
                          haffenden_farm$habitat_name == "Heathland and shrub"] <- 0
  haffenden_farm$net_units[haffenden_farm$section == "off-site" &
                           haffenden_farm$module == "area" &
                           haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.3796

  #off-site woodland and forest
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "off-site" &
                               haffenden_farm$module == "area" &
                               haffenden_farm$habitat_name == "Woodland and forest"] <- 8.349
  haffenden_farm$baseline_units[haffenden_farm$section == "off-site" &
                                haffenden_farm$module == "area" &
                                haffenden_farm$habitat_name == "Woodland and forest"] <- 110.2068
  #post
  haffenden_farm$post_size[haffenden_farm$section == "off-site" &
                           haffenden_farm$module == "area" &
                           haffenden_farm$habitat_name == "Woodland and forest"] <- 8.349
  haffenden_farm$post_units[haffenden_farm$section == "off-site" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Woodland and forest"] <- 119.8630
  #change
  haffenden_farm$net_size[haffenden_farm$section == "off-site" &
                          haffenden_farm$module == "area" &
                          haffenden_farm$habitat_name == "Woodland and forest"] <- 0
  haffenden_farm$net_units[haffenden_farm$section == "off-site" &
                           haffenden_farm$module == "area" &
                           haffenden_farm$habitat_name == "Woodland and forest"] <- 9.6562

  #combined cropland
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "combined" &
                                 haffenden_farm$module == "area" &
                                 haffenden_farm$habitat_name == "Cropland"] <- 3.426
  haffenden_farm$baseline_units[haffenden_farm$section == "combined" &
                                  haffenden_farm$module == "area" &
                                  haffenden_farm$habitat_name == "Cropland"] <- 7.5372
  #change
  haffenden_farm$net_size[haffenden_farm$section == "combined" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Cropland"] <- -3.426
  haffenden_farm$net_units[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Cropland"] <- -7.5372

  #combined grassland
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "combined" &
                                 haffenden_farm$module == "area" &
                                 haffenden_farm$habitat_name == "Grassland"] <- 56.072
  haffenden_farm$baseline_units[haffenden_farm$section == "combined" &
                                  haffenden_farm$module == "area" &
                                  haffenden_farm$habitat_name == "Grassland"] <- 803.1232
  #post
  haffenden_farm$post_size[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Grassland"] <- 59.498
  haffenden_farm$post_units[haffenden_farm$section == "combined" &
                              haffenden_farm$module == "area" &
                              haffenden_farm$habitat_name == "Grassland"] <- 1142.2133

  #change
  haffenden_farm$net_size[haffenden_farm$section == "combined" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Grassland"] <- 3.426
  haffenden_farm$net_units[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Grassland"] <- 339.0901

  #combined heathland and shrub
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "combined" &
                                 haffenden_farm$module == "area" &
                                 haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.096
  haffenden_farm$baseline_units[haffenden_farm$section == "combined" &
                                  haffenden_farm$module == "area" &
                                  haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.8448
  #post
  haffenden_farm$post_size[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.096
  haffenden_farm$post_units[haffenden_farm$section == "combined" &
                              haffenden_farm$module == "area" &
                              haffenden_farm$habitat_name == "Heathland and shrub"] <- 1.2244
  #change
  haffenden_farm$net_size[haffenden_farm$section == "combined" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Heathland and shrub"] <- 0
  haffenden_farm$net_units[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Heathland and shrub"] <- 0.3796

  #combined woodland and forest
  #baseline
  haffenden_farm$baseline_size[haffenden_farm$section == "combined" &
                                 haffenden_farm$module == "area" &
                                 haffenden_farm$habitat_name == "Woodland and forest"] <- 8.349
  haffenden_farm$baseline_units[haffenden_farm$section == "combined" &
                                  haffenden_farm$module == "area" &
                                  haffenden_farm$habitat_name == "Woodland and forest"] <- 110.2068
  #post
  haffenden_farm$post_size[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Woodland and forest"] <- 8.349
  haffenden_farm$post_units[haffenden_farm$section == "combined" &
                              haffenden_farm$module == "area" &
                              haffenden_farm$habitat_name == "Woodland and forest"] <- 119.8630
  #change
  haffenden_farm$net_size[haffenden_farm$section == "combined" &
                            haffenden_farm$module == "area" &
                            haffenden_farm$habitat_name == "Woodland and forest"] <- 0
  haffenden_farm$net_units[haffenden_farm$section == "combined" &
                             haffenden_farm$module == "area" &
                             haffenden_farm$habitat_name == "Woodland and forest"] <- 9.6562

  file_path <- system.file("metric_examples", "haffenden_farm.xlsm", package = "DefraBiodiversityMetric")
  expect_equal(get_detailed_results(file_path), haffenden_farm)
})
