set.seed(123)
skip_if_not(rlang::is_installed("vip"))
train <- vip::gen_friedman(1000, seed = 101)
test <- train[701:1000, ]
train <- train[1:700, ]
comb_rset <- rsample::make_splits(train, test)
comb_rset <- rsample::manual_rset(list(comb_rset), "Fold1")
comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")

pp <- ppr(y ~ ., data = train, nterms = 11)
importance <- vip::vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)

test_that("`ww_area_of_applicability` is properly classed", {
  model <- ww_area_of_applicability(y ~ ., train, test, importance)
  expect_s3_class(model, "ww_area_of_applicability")
  expect_s3_class(model, "hardhat_model")
})


test_that("`ww_area_of_applicability` is not defined for vectors", {
  expect_snapshot_error(
    ww_area_of_applicability(mtcars$mpg)
  )
})

test_that("`ww_area_of_applicability` finds 0 distance between identical data", {

  expect_equal(
    ww_area_of_applicability(y ~ ., train, train, importance)$aoa_threshold,
    0,
    tolerance = 1e-7
  )

})

test_that("`ww_area_of_applicability` works with or without a testing set", {

  expect_error(
    ww_area_of_applicability(y ~ ., train, test, importance),
    NA
  )

  expect_error(
    ww_area_of_applicability(y ~ ., train, importance = importance),
    NA
  )

})

test_that("`ww_area_of_applicability` methods are equivalent", {

  methods <- list(
    ww_area_of_applicability(y ~ ., train, test, importance),
    ww_area_of_applicability(train[2:11], test[2:11], importance),
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance),
    ww_area_of_applicability(comb_rset_no_y, importance = importance)
  )

  expect_identical(
    head(methods[[1]], -1),
    head(methods[[2]], -1)
  )

  expect_identical(
    predict(methods[[1]], test),
    predict(methods[[2]], test)
  )

  expect_identical(
    head(methods[[2]], -1),
    head(methods[[3]], -1)
  )

  expect_identical(
    predict(methods[[2]], test),
    predict(methods[[3]], test)
  )

  # Comparing rset method to the others --
  # because here we calculate our training data on the entire thing
  # the training, means, sds slots are all different
  expect_identical(
    methods[[3]]$aoa_threshold,
    methods[[4]]$aoa_threshold
  )

  skip_if_not_installed("recipes")
  methods[[5]] <- ww_area_of_applicability(
    comb_rset,
    recipes::recipe(y ~ ., train),
    importance = importance
  )
  expect_identical(
    head(methods[[4]], -1),
    head(methods[[5]], -1)
  )

  expect_identical(
    predict(methods[[4]], test),
    predict(methods[[5]], test)
  )


})

test_that("`ww_area_of_applicability` can handle different column orders", {

  expect_equal(
    ww_area_of_applicability(train[2:11], test[2:11], importance)$aoa_threshold,
    ww_area_of_applicability(train[2:11], test[11:2], importance)$aoa_threshold
  )

  expect_equal(
    ww_area_of_applicability(train[2:11], test[2:11], importance)$aoa_threshold,
    ww_area_of_applicability(train[11:2], test[2:11], importance)$aoa_threshold
  )

})

test_that("NAs are handled", {

  train[1, 2] <- NA
  test[1, 2] <- NA
  comb_rset <- rsample::make_splits(train, test)
  comb_rset <- rsample::manual_rset(list(comb_rset), "Fold1")
  comb_rset_no_y <- rsample::make_splits(train[2:11], test[2:11])
  comb_rset_no_y <- rsample::manual_rset(list(comb_rset_no_y), "Fold1")

  expect_snapshot_error(
    ww_area_of_applicability(y ~ ., train, test, importance)
  )
  expect_snapshot(
    ww_area_of_applicability(y ~ ., train, test, importance, na_action = na.omit)
  )

  expect_snapshot_error(
    ww_area_of_applicability(train[2:11], test[2:11], importance)
  )
  expect_snapshot(
    ww_area_of_applicability(train[2:11], test[2:11], importance, na_action = na.omit)
  )

  expect_snapshot_error(
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance)
  )
  expect_snapshot(
    ww_area_of_applicability(as.matrix(train[2:11]), as.matrix(test[2:11]), importance, na_action = na.omit)
  )

  expect_snapshot_error(
    ww_area_of_applicability(comb_rset_no_y, importance = importance)
  )
  expect_snapshot(
    ww_area_of_applicability(comb_rset_no_y, importance = importance, na_action = na.omit)
  )

  skip_if_not_installed("recipes")
  expect_snapshot_error(
    ww_area_of_applicability(
      comb_rset,
      recipes::recipe(y ~ ., train),
      importance = importance
    )
  )
  expect_snapshot(
    ww_area_of_applicability(
      comb_rset,
      recipes::recipe(y ~ ., train),
      importance = importance,
      na_action = na.omit
    )
  )

  expect_snapshot(
    predict(
      ww_area_of_applicability(y ~ ., train, test, importance, na_action = na.omit),
      test
    )
  )

})

skip_if_not(rlang::is_installed("vip"))
train <- vip::gen_friedman(1000, seed = 101)
test <- train[701:1000, ]
train <- train[1:700, ]

pp <- ppr(y ~ ., data = train, nterms = 11)
importance <- vip::vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)
aoa <- ww_area_of_applicability(y ~ ., train, test, importance)

test_that("normal use", {

  expect_snapshot(
    predict(aoa, test)
  )

  skip_on_os("mac")
  expect_snapshot(
    predict(aoa, train)
  )

})

test_that("`new_ww_area_of_applicability` arguments are assigned correctly", {
  x <- ww_area_of_applicability(y ~ ., train, test, importance)

  skip_on_os("mac")
  expect_equal(names(x), c("transformed_training", "sds", "means", "importance", "d_bar", "aoa_threshold", "blueprint"))
  expect_snapshot(x$transformed_training)
  expect_snapshot(x$sds)
  expect_snapshot(x$means)
  expect_snapshot(x$importance)
  expect_snapshot(x$d_bar)
  expect_snapshot(x$aoa_threshold)
  expect_snapshot(x$blueprint)
  expect_s3_class(x$blueprint, "hardhat_blueprint")
})

