test_that("make sankey works", {
  files <- list.files(system.file("extdata", package = "defraBiodiversityMetric"))
  for (i in 1:length(files)) {

    filepath <- system.file("extdata", files[i], package = "defraBiodiversityMetric")
    sankey <- defraBiodiversityMetric::make_sankey(filepath, section = "off-site", module = "area")
    show(sankey)
    expect_equal(class(sankey)[1], "sankeyNetwork")

  }
})
