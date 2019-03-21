library(eenv)
context("Test test_get_* functions")

test_that("test_get_[rows] are returning the correct results", {
  test <- tibble::tibble(
    pred = c("yes", "yes", "yes",  "no",  "no",  "no",  "no",  "no",  "no",  "no"),
    act  = c("yes",  "no",  "no", "yes", "yes", "yes",  "no",  "no",  "no",  "no"))

  # true positives
  expect_setequal(
    test_get_true_positives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["pred"]],
    "yes")
  expect_setequal(
    test_get_true_positives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["act"]],
    "yes")
  # false positives
  expect_setequal(
    test_get_false_positives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["pred"]],
    "yes")
  expect_setequal(
    test_get_false_positives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["act"]],
    "no")
  # actual positives
  expect_setequal(
    test_get_actual_positives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["act"]],
    "yes")
  # predicted positives
  expect_setequal(
    test_get_false_positives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["pred"]],
    "yes")
  # true negatives
  expect_setequal(
    test_get_true_negatives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["pred"]],
    "no")
  expect_setequal(
    test_get_true_negatives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["act"]],
    "no")
  # false negatives
  expect_setequal(
    test_get_false_negatives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["pred"]],
    "no")
  expect_setequal(
    test_get_false_negatives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["act"]],
    "yes")
  # actual negatives
  expect_setequal(
    test_get_actual_negatives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["act"]],
    "no")
  # predicted negatives
  expect_setequal(
    test_get_false_negatives(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")[["pred"]],
    "no")
})

test_that("test_get_[values] are returning the correct results", {
  test <- tibble::tibble(
    pred = c("yes", "yes", "yes",  "no",  "no",  "no",  "no",  "no",  "no",  "no"),
    act  = c("yes",  "no",  "no", "yes", "yes", "yes",  "no",  "no",  "no",  "no"))

  # confusion matrix
  expect_equivalent(
    test_get_confusion_matrix(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes"),
    matrix(c(1, 2, 3, 4), byrow = TRUE, ncol = 2))
  # sensitivity
  expect_equal(
    test_get_sensitivity(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes"),
    1 / (1 + 3))
  # specificity
  expect_setequal(
    test_get_specificity(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes"),
    4 / (4 + 2))
  # prevalence
  expect_equal(
    test_get_prevalence(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes"),
    (1 + 3) / (1 + 2 + 3 + 4))
  # ppv
  expect_equal(
    test_get_positive_predictive_value(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes"),
    1 / (1 + 2))
  # npv
  expect_equal(
    test_get_negative_predictive_value(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes"),
    4 / (4 + 3))
})

#test_get_metrics(data = test, pred_cond = pred, act_cond = act, pred_cond_targ = "yes", act_cond_targ = "yes")
