library("uuid")
library("rjson")
#' Prediction client
#'
#' Prediction Client to help capture prediction data
#' @import methods
#' @export PredictionClient
#' @title Prediction Client Class
#' @field feature_names array of feature names
#' @field predict_names array of prediction names
#' @field metadata_names array of metadata names
#' @field instance_id string containing a unique instance id

#' @examples
#' library(DominoPredictionLogging)
#' prediction_client <- PredictionClient(
#' feature_names = c("sepal.length", "sepal.width", "petal.length"),
#' predict_names = c("variety")
#' )
#' prediction_client$record(
#' feature_values = c(sepal_length, sepal_width, petal_length),
#' predict_values = c(predicted_value),
#' event_id = event_id
#' )

PredictionClient <- setRefClass( # nolint
  "PredictionClient",
  fields = list(
    feature_names = "vector",
    predict_names = "vector",
    metadata_names = "vector",
    instance_id = "character"
  ),
  methods = list(
    record = function(feature_values,
                      predict_values,
                      metadata_values = NULL,
                      event_id = NULL,
                      timestamp = NULL,
                      pred_prob = NULL,
                      sample_wt = NULL) {
      data <- list()
      tryCatch({
        error_prefix <- "ERROR DominoPredictionLogging::log_predictions"
        # preconditions
        if (length(feature_names) != length(feature_values)) {
          print(paste(
            error_prefix,
            "feature_names and feature_values should be of same length"
          ))
          return(data)
        }
        if (length(predict_names) != length(predict_values)) {
          print(paste(
            error_prefix,
            "predict_names and predict_values should be of same length"
          ))
          return(data)
        }
        if (!missing(metadata_values) &&
          (length(metadata_names) != length(metadata_values))) {
          print(paste(
            error_prefix,
            "metadata_names and metadata_values should be of same length"
          ))
          return(data)
        }

        data$features <- feature_values
        names(data$features) <- feature_names

        data$predictions <- predict_values
        names(data$predictions) <- predict_names

        if (!missing(metadata_values)) {
          data$metadata <- metadata_values
          names(data$metadata) <- metadata_names
        }

        if (missing(event_id)) {
          event_id <- uuid::UUIDgenerate()
        }
        if (missing(timestamp)) {
          timestamp <- get_current_timestamp()
        }
        data$"__domino_timestamp" <- get_current_timestamp() # nolint
        data$event_id <- event_id
        data$timestamp <- timestamp

        data$instance_id <- get_instance_id(instance_id)

        if (!missing(pred_prob)) {
          data$prediction_probability <- pred_prob
        }
        if (!missing(sample_wt)) {
          data$sample_weight <- sample_wt
        }
        prediction_client_logger <- init_logger()
        prediction_client_logger$info(rjson::toJSON(data))
      },
        error = function(error_msg) {
          print(paste(error_prefix, error_msg))
        }
      )
      return(data)
    }
  )
)
