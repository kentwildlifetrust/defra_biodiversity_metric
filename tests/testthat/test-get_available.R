test_that("available units extract", {
  for (file in list.files(system.file("metric_examples", package = "DefraBiodiversityMetric"))) {
    file_path <- system.file("metric_examples", file, package = "DefraBiodiversityMetric")
    expect_equal(class(get_available(file_path, section = "off-site", module = "area")), "data.frame")
    expect_equal(class(get_available(file_path, section = "off-site", module = "hedgerow")), "data.frame")
    expect_equal(class(get_available(file_path, section = "off-site", module = "watercourse")), "data.frame")
    expect_equal(class(get_available(file_path, section = "on-site", module = "area")), "data.frame")
    expect_equal(class(get_available(file_path, section = "on-site", module = "hedgerow")), "data.frame")
    expect_equal(class(get_available(file_path, section = "on-site", module = "watercourse")), "data.frame")
  }
})


test_that("expected total available units", {
  for (file in list.files(system.file("metric_examples", package = "DefraBiodiversityMetric"))) {
    file_path <- system.file("metric_examples", file, package = "DefraBiodiversityMetric")

    for (section in c("off-site", "on-site")) {
      for (module in c("area", "hedgerow", "watercourse")) {
        s <- section
        m <- module
        available <- get_available(file_path, section = section, module = module) %>%
          pull(available_units) %>%
          sum() %>%
          round(2)
        expected <- get_detailed_results(file_path) %>%
          dplyr::filter(section == s & module == m) %>%
          dplyr::summarise(sum(net_units)) %>%
          dplyr::pull() %>%
          round(2)
        expect_equal(available, expected)
      }
    }
  }
})
