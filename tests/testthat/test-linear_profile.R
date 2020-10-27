context('Linear profile plotting')

library(pRolocdata)
data(lopitdcU2OS2018)

test_that("with replicates", {

  p <- plot_feature_profiles(head(lopitdcU2OS2018), c(10,20))

  vdiffr::expect_doppelganger("linear_feature_profiles_with_replicates", p)
})


