if (requireNamespace("lintr", quietly = TRUE)) {
  context("Lints -- Produce female code")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
