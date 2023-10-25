test_that("toc_nrows", {
  x<-tempo_toc()
    expect_equal(nrow(x), 1785)
})

test_that("toc_ncols", {
  x<-tempo_toc()
  expect_equal(ncol(x), 2)
})

test_that("toc_names", {
  x<-tempo_toc()
  expect_equal(names(x), c("name", "code"))
})

test_that("toc_nrows_desc", {
  x<-tempo_toc(full_description = TRUE)
  expect_equal(nrow(x), 1785)
})

test_that("toc_ncols_desc", {
  x<-tempo_toc(full_description = TRUE)
  expect_equal(ncol(x), 6)
})

test_that("toc_names_desc", {
  x<-tempo_toc(full_description = TRUE)
  expect_equal(names(x), c("name", "code", "Statistical_domain", "Statistical_sub_domain", "Survey_name", "Last_update"))
})

test_that("toc_code", {
  x<-tempo_toc()
  expect_equal(x[1, "code"], "ACC101A")
})

test_that("toc_code_length", {
  x<-tempo_toc()
  lg <- lapply(x[,2], nchar)
  expect_equal(lg, rep(list(7), nrow(x)))
})
