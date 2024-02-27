test_that("baseline results extract", {
  for (file in list.files(system.file("extdata", package = "defraBiodiversityMetric"))) {
    file_path <- system.file("extdata", file, package = "defraBiodiversityMetric")
    expect_equal(class(get_baseline(file_path, section = "off-site", module = "area")), "data.frame")
    expect_equal(class(get_baseline(file_path, section = "off-site", module = "hedgerow")), "data.frame")
    expect_equal(class(get_baseline(file_path, section = "off-site", module = "watercourse")), "data.frame")
    expect_equal(class(get_baseline(file_path, section = "on-site", module = "area")), "data.frame")
    expect_equal(class(get_baseline(file_path, section = "on-site", module = "hedgerow")), "data.frame")
    expect_equal(class(get_baseline(file_path, section = "on-site", module = "watercourse")), "data.frame")
  }
})
