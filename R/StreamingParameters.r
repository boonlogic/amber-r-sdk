# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' StreamingParameters Class
#'
#' @field anomalyHistoryWindow 
#' @field learningRateNumerator 
#' @field learningRateDenominator 
#' @field learningMaxClusters 
#' @field learningMaxSamples 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
StreamingParameters <- R6::R6Class(
  'StreamingParameters',
  public = list(
    `anomalyHistoryWindow` = NULL,
    `learningRateNumerator` = NULL,
    `learningRateDenominator` = NULL,
    `learningMaxClusters` = NULL,
    `learningMaxSamples` = NULL,
    initialize = function(`anomalyHistoryWindow`, `learningRateNumerator`, `learningRateDenominator`, `learningMaxClusters`, `learningMaxSamples`){
      if (!missing(`anomalyHistoryWindow`)) {
        stopifnot(is.numeric(`anomalyHistoryWindow`), length(`anomalyHistoryWindow`) == 1)
        stopifnot(R6::is.R6(`anomalyHistoryWindow`))
        self$`anomalyHistoryWindow` <- `anomalyHistoryWindow`
      }
      if (!missing(`learningRateNumerator`)) {
        stopifnot(is.numeric(`learningRateNumerator`), length(`learningRateNumerator`) == 1)
        stopifnot(R6::is.R6(`learningRateNumerator`))
        self$`learningRateNumerator` <- `learningRateNumerator`
      }
      if (!missing(`learningRateDenominator`)) {
        stopifnot(is.numeric(`learningRateDenominator`), length(`learningRateDenominator`) == 1)
        stopifnot(R6::is.R6(`learningRateDenominator`))
        self$`learningRateDenominator` <- `learningRateDenominator`
      }
      if (!missing(`learningMaxClusters`)) {
        stopifnot(is.numeric(`learningMaxClusters`), length(`learningMaxClusters`) == 1)
        stopifnot(R6::is.R6(`learningMaxClusters`))
        self$`learningMaxClusters` <- `learningMaxClusters`
      }
      if (!missing(`learningMaxSamples`)) {
        stopifnot(is.numeric(`learningMaxSamples`), length(`learningMaxSamples`) == 1)
        stopifnot(R6::is.R6(`learningMaxSamples`))
        self$`learningMaxSamples` <- `learningMaxSamples`
      }
    },
    toJSON = function() {
      StreamingParametersObject <- list()
      if (!is.null(self$`anomalyHistoryWindow`)) {
        StreamingParametersObject[['anomalyHistoryWindow']] <- self$`anomalyHistoryWindow`$toJSON()
      }
      if (!is.null(self$`learningRateNumerator`)) {
        StreamingParametersObject[['learningRateNumerator']] <- self$`learningRateNumerator`$toJSON()
      }
      if (!is.null(self$`learningRateDenominator`)) {
        StreamingParametersObject[['learningRateDenominator']] <- self$`learningRateDenominator`$toJSON()
      }
      if (!is.null(self$`learningMaxClusters`)) {
        StreamingParametersObject[['learningMaxClusters']] <- self$`learningMaxClusters`$toJSON()
      }
      if (!is.null(self$`learningMaxSamples`)) {
        StreamingParametersObject[['learningMaxSamples']] <- self$`learningMaxSamples`$toJSON()
      }

      StreamingParametersObject
    },
    fromJSON = function(StreamingParametersJson) {
      StreamingParametersObject <- jsonlite::fromJSON(StreamingParametersJson)
      if (!is.null(StreamingParametersObject$`anomalyHistoryWindow`)) {
        anomalyHistoryWindowObject <- BigDecimal$new()
        anomalyHistoryWindowObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$anomalyHistoryWindow, auto_unbox = TRUE))
        self$`anomalyHistoryWindow` <- anomalyHistoryWindowObject
      }
      if (!is.null(StreamingParametersObject$`learningRateNumerator`)) {
        learningRateNumeratorObject <- BigDecimal$new()
        learningRateNumeratorObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningRateNumerator, auto_unbox = TRUE))
        self$`learningRateNumerator` <- learningRateNumeratorObject
      }
      if (!is.null(StreamingParametersObject$`learningRateDenominator`)) {
        learningRateDenominatorObject <- BigDecimal$new()
        learningRateDenominatorObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningRateDenominator, auto_unbox = TRUE))
        self$`learningRateDenominator` <- learningRateDenominatorObject
      }
      if (!is.null(StreamingParametersObject$`learningMaxClusters`)) {
        learningMaxClustersObject <- BigDecimal$new()
        learningMaxClustersObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningMaxClusters, auto_unbox = TRUE))
        self$`learningMaxClusters` <- learningMaxClustersObject
      }
      if (!is.null(StreamingParametersObject$`learningMaxSamples`)) {
        learningMaxSamplesObject <- BigDecimal$new()
        learningMaxSamplesObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningMaxSamples, auto_unbox = TRUE))
        self$`learningMaxSamples` <- learningMaxSamplesObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "anomalyHistoryWindow": %s,
           "learningRateNumerator": %s,
           "learningRateDenominator": %s,
           "learningMaxClusters": %s,
           "learningMaxSamples": %s
        }',
        self$`anomalyHistoryWindow`$toJSON(),
        self$`learningRateNumerator`$toJSON(),
        self$`learningRateDenominator`$toJSON(),
        self$`learningMaxClusters`$toJSON(),
        self$`learningMaxSamples`$toJSON()
      )
    },
    fromJSONString = function(StreamingParametersJson) {
      StreamingParametersObject <- jsonlite::fromJSON(StreamingParametersJson)
      BigDecimalObject <- BigDecimal$new()
      self$`anomalyHistoryWindow` <- BigDecimalObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$anomalyHistoryWindow, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`learningRateNumerator` <- BigDecimalObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningRateNumerator, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`learningRateDenominator` <- BigDecimalObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningRateDenominator, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`learningMaxClusters` <- BigDecimalObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningMaxClusters, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`learningMaxSamples` <- BigDecimalObject$fromJSON(jsonlite::toJSON(StreamingParametersObject$learningMaxSamples, auto_unbox = TRUE))
    }
  )
)
