test_that("enhancement details extract", {
  for (file in list.files(system.file("metric_examples", package = "DefraBiodiversityMetric"))) {
    file_path <- system.file("metric_examples", file, package = "DefraBiodiversityMetric")
    expect_equal(class(get_enhancement(file_path, section = "off-site", module = "area")), "data.frame")
    expect_equal(class(get_enhancement(file_path, section = "off-site", module = "hedgerow")), "data.frame")
    expect_equal(class(get_enhancement(file_path, section = "off-site", module = "watercourse")), "data.frame")
    expect_equal(class(get_enhancement(file_path, section = "on-site", module = "area")), "data.frame")
    expect_equal(class(get_enhancement(file_path, section = "on-site", module = "hedgerow")), "data.frame")
    expect_equal(class(get_enhancement(file_path, section = "on-site", module = "watercourse")), "data.frame")
  }
})
