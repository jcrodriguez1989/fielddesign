test_that("fielddesign_app - mock execution", {
  local_mocked_bindings(
    runGadget = function(...) NULL,
    .package = "fielddesign"
  )
  expect_null(fielddesign_app())
})
