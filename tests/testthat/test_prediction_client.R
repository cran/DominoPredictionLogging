library(testthat)
library("rjson")
context("prediciton client")

test_that("check when all the data is passed", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list(
    features = c(min = 1, max = 100),
    predictions = c("prediction" = 20),
    metadata = c("meta1" = "metadata_value1"),
    instance_id = "test_instance_id",
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    prediction_probability = c(0.1, 0.9),
    sample_weight = 0.3
  )
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )

  expect_true("__domino_timestamp" %in% names(actual_data))
  actual_data$"__domino_timestamp" <- NULL # nolint
  expect_true(setequal(expected_data, actual_data))
})

test_that("check whether the data is written to the file", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  features <- c(min = 1, max = 100)
  predictions <- c("prediction" = 20)
  metadata <- c("meta1" = "metadata_value1")
  instance_id <- "test_instance_id"
  event_id <- "test_event_id"
  timestamp <- "adfasdfasd"
  prediction_probability <- c(0.1, 0.9)
  sample_weight <- 0.3
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  Sys.setenv("HOSTNAME" = "model-7100391064067162909bf2f2-7d75566cb-49l9j")
  tmp_file <- get_log_file()
  file.remove(tmp_file)
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )
  json_data_written_to_file <- fromJSON(file = tmp_file)
  expect_true(setequal(json_data_written_to_file$features, features))
  expect_true(setequal(json_data_written_to_file$predictions, predictions))
  expect_true(setequal(json_data_written_to_file$metadata, metadata))
  expect_true(setequal(json_data_written_to_file$instance_id, instance_id))
  expect_true(setequal(json_data_written_to_file$event_id, event_id))
  expect_true(setequal(json_data_written_to_file$timestamp, timestamp))
  expect_true(setequal(
    json_data_written_to_file$prediction_probability,
    prediction_probability
  ))
  expect_true(setequal(json_data_written_to_file$sample_weight, sample_weight))
  Sys.unsetenv("HOSTNAME")
  Sys.unsetenv("PREDICTION_DATA_DIRECTORY")
})

test_that("check whether event_id is getting generated if its not passed", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    timestamp = "adfasdfasd",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )
  expect_true("event_id" %in% names(actual_data))
})

test_that("not throw error when log file has no write permissions", {
  Sys.setenv("HOSTNAME" = "model-7100391064067162909bf2f2-7d75566cb-49l9j")
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/random_folder")
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list(
    features = c(min = 1, max = 100),
    predictions = c("prediction" = 20),
    metadata = c("meta1" = "metadata_value1"),
    instance_id = "test_instance_id",
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    prediction_probability = c(0.1, 0.9),
    sample_weight = 0.3
  )
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )
  actual_data$"__domino_timestamp" <- NULL # nolint
  expect_true(setequal(expected_data, actual_data))
  Sys.unsetenv("HOSTNAME")
  Sys.unsetenv("PREDICTION_DATA_DIRECTORY")
})


test_that("check whether model_version_id is collected from env variables", {
  expected_instance_id <- "6100391064067162909bf2f2"
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  Sys.setenv("HOSTNAME" = "model-6100391064067162909bf2f2-7d75566cb-49l9j")
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1")
  )
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    timestamp = "adfasdfasd",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )
  expect_equal(actual_data$instance_id, expected_instance_id)
})

test_that("check whether timestamp is added if its not passed", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )
  expect_true("timestamp" %in% names(actual_data))
})

test_that("check whether the output is correct when metadata is empty", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )

  expected_data <- list(
    features = c(min = 1, max = 100),
    predictions = c("prediction" = 20),
    instance_id = "test_instance_id",
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    prediction_probability = c(0.1, 0.9),
    sample_weight = 0.3
  )
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    event_id = "test_event_id",
    timestamp = "adfasdfasd",
    pred_prob = c(0.1, 0.9),
    sample_wt = 0.3
  )
  actual_data$"__domino_timestamp" <- NULL # nolint
  expect_true(setequal(expected_data, actual_data))
})

test_that(
  "check when there are no prediction probability and sample weights columns", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list(
    features = c(min = 1, max = 100),
    predictions = c("prediction" = 20),
    metadata = c("meta1" = "metadata_value1"),
    instance_id = "test_instance_id",
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  actual_data$"__domino_timestamp" <- NULL # nolint
  expect_true(setequal(expected_data, actual_data))
})

test_that("check when feature_values are less than feature_names", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list()
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  expect_true(setequal(expected_data, actual_data))
})
test_that("check when feature_values more than feature_names", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list()
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 50, 100),
    predict_values = c(20),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  expect_true(setequal(expected_data, actual_data))
})

test_that("check when prediction_values are less than prediction_names", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list()
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  expect_true(setequal(expected_data, actual_data))
})

test_that("check when prediction_values are more than prediction_names", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list()
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20, 30),
    metadata_values = c("metadata_value1"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  expect_true(setequal(expected_data, actual_data))
})


test_that("check when metadata_values are less than metadata_names", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list()
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20),
    metadata_values = c(),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  expect_true(setequal(expected_data, actual_data))
})

test_that("check when metadata_values are more than metadata_names", {
  prediction_client <- PredictionClient(
    feature_names = c("min", "max"),
    predict_names = c("prediction"),
    metadata_names = c("meta1"),
    instance_id = "test_instance_id"
  )
  expected_data <- list()
  Sys.setenv("PREDICTION_DATA_DIRECTORY" = "/tmp")
  actual_data <- prediction_client$record(
    feature_values = c(1, 100),
    predict_values = c(20, 30),
    metadata_values = c("metadata_value1", "metadata_value2"),
    event_id = "test_event_id",
    timestamp = "adfasdfasd"
  )
  expect_true(setequal(expected_data, actual_data))
})
