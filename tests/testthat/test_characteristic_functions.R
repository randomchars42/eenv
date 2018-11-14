library(eenv)
context("Test characteristic functions")

test_that("characteristics are calculated correctly", {
  data_test <- tibble::tibble(
    id = 1 : 12,
    sex = c("f", "f", "f", "m", "m", "f", "f", "m", "m", "f", "m", "f"), # 12/7/5 (t/f/m)
    group = c("a", "b", "a", "b", "a", "b", "a", "b", "a", "b", "a", "b"), # a: 6/3/3 (t/f/m), b: 6/4/2 (t/f/m)
    group2 = c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c"),  # a: 4/3/1 (t/f/m), b: 4/1/3 (t/f/m) c: 4/3/1(t/f/m)
    value = c(2, 5, 6, 3, 9, 12, 2, 5, 6, 3, 9, 12))

  expect_equal(
    unname(characteristic_calc_count(
      data = data_test$sex,
      result = c(),
      events = "f")),
    7)
  expect_equal(
    unname(characteristic_calc_mean(
      data = c(3, 5, 5, 3, 5, 5, 5, 3, 3, 3),
      result = c())),
    4)
  #characteristic_get(data = data_test, characteristic = sex, group, group2, events = c("f"), template = "%c (%pt)")
  #characteristic_get(data = data_test, characteristic = value, group, group2, quantiles = c("q10", "q5", "q25", "q50", "q75", "q75"), template = "%q50 (%q25 - %q75)", decimals = 0)
})
