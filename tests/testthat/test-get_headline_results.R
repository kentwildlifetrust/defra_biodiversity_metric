test_that("headline results extract", {
  for (file in list.files(system.file("metric_examples", package = "DefraBiodiversityMetric"))) {
    file_path <- system.file("metric_examples", file, package = "DefraBiodiversityMetric")
    expect_equal(class(get_headline_results(file_path)), "data.frame")
  }
})

template_df <- data.frame(
  section = c(rep("off-site", 9),
              rep("on-site", 9)),
  module = rep(rep(c("area", "hedgerow", "watercourse"), each = 3), 2),
  type = rep(c("base", "net", "post"), 6),
  units = rep(0, 18)
)

test_that("headline results get extracted correctly", {
  haffenden_farm <- template_df
  haffenden_farm$units[haffenden_farm$section == "off-site" &
                         haffenden_farm$module == "area" &
                         haffenden_farm$type == "base"] <- 921.7120
  haffenden_farm$units[haffenden_farm$section == "off-site" &
                         haffenden_farm$module == "area" &
                         haffenden_farm$type == "post"] <- 1263.3008
  haffenden_farm$units[haffenden_farm$section == "off-site" &
                         haffenden_farm$module == "area" &
                         haffenden_farm$type == "net"] <- 341.5888

  file_path <- system.file("metric_examples", "haffenden_farm.xlsm", package = "DefraBiodiversityMetric")
  expect_equal(get_headline_results(file_path), haffenden_farm)
})


