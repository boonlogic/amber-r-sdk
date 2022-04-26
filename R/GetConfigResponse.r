# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GetConfigResponse Class
#'
#' @field anomalyHistoryWindow 
#' @field learningRateNumerator 
#' @field learningRateDenominator 
#' @field learningMaxClusters 
#' @field learningMaxSamples 
#' @field featureCount 
#' @field streamingWindowSize 
#' @field features 
#' @field percentVariation 
#' @field samplesToBuffer 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GetConfigResponse <- R6::R6Class("GetConfigResponse", public = list(anomalyHistoryWindow = NULL,
    learningRateNumerator = NULL, learningRateDenominator = NULL, learningMaxClusters = NULL,
    learningMaxSamples = NULL, featureCount = NULL, streamingWindowSize = NULL, features = NULL,
    percentVariation = NULL, samplesToBuffer = NULL, initialize = function(anomalyHistoryWindow,
        learningRateNumerator, learningRateDenominator, learningMaxClusters, learningMaxSamples,
        featureCount, streamingWindowSize, features, percentVariation, samplesToBuffer) {
        if (!missing(anomalyHistoryWindow)) {
            stopifnot(is.numeric(anomalyHistoryWindow), length(anomalyHistoryWindow) ==
                1)
            stopifnot(R6::is.R6(anomalyHistoryWindow))
            self$anomalyHistoryWindow <- anomalyHistoryWindow
        }
        if (!missing(learningRateNumerator)) {
            stopifnot(is.numeric(learningRateNumerator), length(learningRateNumerator) ==
                1)
            stopifnot(R6::is.R6(learningRateNumerator))
            self$learningRateNumerator <- learningRateNumerator
        }
        if (!missing(learningRateDenominator)) {
            stopifnot(is.numeric(learningRateDenominator), length(learningRateDenominator) ==
                1)
            stopifnot(R6::is.R6(learningRateDenominator))
            self$learningRateDenominator <- learningRateDenominator
        }
        if (!missing(learningMaxClusters)) {
            stopifnot(is.numeric(learningMaxClusters), length(learningMaxClusters) ==
                1)
            stopifnot(R6::is.R6(learningMaxClusters))
            self$learningMaxClusters <- learningMaxClusters
        }
        if (!missing(learningMaxSamples)) {
            stopifnot(is.numeric(learningMaxSamples), length(learningMaxSamples) ==
                1)
            stopifnot(R6::is.R6(learningMaxSamples))
            self$learningMaxSamples <- learningMaxSamples
        }
        if (!missing(featureCount)) {
            stopifnot(is.numeric(featureCount), length(featureCount) == 1)
            self$featureCount <- featureCount
        }
        if (!missing(streamingWindowSize)) {
            stopifnot(is.numeric(streamingWindowSize), length(streamingWindowSize) ==
                1)
            self$streamingWindowSize <- streamingWindowSize
        }
        if (!missing(features)) {
            stopifnot(is.list(features), length(features) != 0)
            lapply(features, function(x) stopifnot(R6::is.R6(x)))
            self$features <- features
        }
        if (!missing(percentVariation)) {
            stopifnot(is.numeric(percentVariation), length(percentVariation) == 1)
            self$percentVariation <- percentVariation
        }
        if (!missing(samplesToBuffer)) {
            stopifnot(is.numeric(samplesToBuffer), length(samplesToBuffer) == 1)
            stopifnot(R6::is.R6(samplesToBuffer))
            self$samplesToBuffer <- samplesToBuffer
        }
    }, toJSON = function() {
        GetConfigResponseObject <- list()
        if (!is.null(self$anomalyHistoryWindow)) {
            GetConfigResponseObject[["anomalyHistoryWindow"]] <- self$anomalyHistoryWindow$toJSON()
        }
        if (!is.null(self$learningRateNumerator)) {
            GetConfigResponseObject[["learningRateNumerator"]] <- self$learningRateNumerator$toJSON()
        }
        if (!is.null(self$learningRateDenominator)) {
            GetConfigResponseObject[["learningRateDenominator"]] <- self$learningRateDenominator$toJSON()
        }
        if (!is.null(self$learningMaxClusters)) {
            GetConfigResponseObject[["learningMaxClusters"]] <- self$learningMaxClusters$toJSON()
        }
        if (!is.null(self$learningMaxSamples)) {
            GetConfigResponseObject[["learningMaxSamples"]] <- self$learningMaxSamples$toJSON()
        }
        if (!is.null(self$featureCount)) {
            GetConfigResponseObject[["featureCount"]] <- self$featureCount
        }
        if (!is.null(self$streamingWindowSize)) {
            GetConfigResponseObject[["streamingWindowSize"]] <- self$streamingWindowSize
        }
        if (!is.null(self$features)) {
            GetConfigResponseObject[["features"]] <- lapply(self$features, function(x) x$toJSON())
        }
        if (!is.null(self$percentVariation)) {
            GetConfigResponseObject[["percentVariation"]] <- self$percentVariation
        }
        if (!is.null(self$samplesToBuffer)) {
            GetConfigResponseObject[["samplesToBuffer"]] <- self$samplesToBuffer$toJSON()
        }

        GetConfigResponseObject
    }, fromJSON = function(GetConfigResponseJson) {
        GetConfigResponseObject <- jsonlite::fromJSON(GetConfigResponseJson)
        if (!is.null(GetConfigResponseObject$anomalyHistoryWindow)) {
            anomalyHistoryWindowObject <- BigDecimal$new()
            anomalyHistoryWindowObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$anomalyHistoryWindow,
                auto_unbox = TRUE))
            self$anomalyHistoryWindow <- anomalyHistoryWindowObject
        }
        if (!is.null(GetConfigResponseObject$learningRateNumerator)) {
            learningRateNumeratorObject <- BigDecimal$new()
            learningRateNumeratorObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningRateNumerator,
                auto_unbox = TRUE))
            self$learningRateNumerator <- learningRateNumeratorObject
        }
        if (!is.null(GetConfigResponseObject$learningRateDenominator)) {
            learningRateDenominatorObject <- BigDecimal$new()
            learningRateDenominatorObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningRateDenominator,
                auto_unbox = TRUE))
            self$learningRateDenominator <- learningRateDenominatorObject
        }
        if (!is.null(GetConfigResponseObject$learningMaxClusters)) {
            learningMaxClustersObject <- BigDecimal$new()
            learningMaxClustersObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningMaxClusters,
                auto_unbox = TRUE))
            self$learningMaxClusters <- learningMaxClustersObject
        }
        if (!is.null(GetConfigResponseObject$learningMaxSamples)) {
            learningMaxSamplesObject <- BigDecimal$new()
            learningMaxSamplesObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningMaxSamples,
                auto_unbox = TRUE))
            self$learningMaxSamples <- learningMaxSamplesObject
        }
        if (!is.null(GetConfigResponseObject$featureCount)) {
            self$featureCount <- GetConfigResponseObject$featureCount
        }
        if (!is.null(GetConfigResponseObject$streamingWindowSize)) {
            self$streamingWindowSize <- GetConfigResponseObject$streamingWindowSize
        }
        if (!is.null(GetConfigResponseObject$features)) {
            self$features <- lapply(GetConfigResponseObject$features, function(x) {
                featuresObject <- FeatureConfig$new()
                featuresObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
                featuresObject
            })
        }
        if (!is.null(GetConfigResponseObject$percentVariation)) {
            self$percentVariation <- GetConfigResponseObject$percentVariation
        }
        if (!is.null(GetConfigResponseObject$samplesToBuffer)) {
            samplesToBufferObject <- BigDecimal$new()
            samplesToBufferObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$samplesToBuffer,
                auto_unbox = TRUE))
            self$samplesToBuffer <- samplesToBufferObject
        }
    }, toJSONString = function() {
        sprintf("{
           \"anomalyHistoryWindow\": %s,
           \"learningRateNumerator\": %s,
           \"learningRateDenominator\": %s,
           \"learningMaxClusters\": %s,
           \"learningMaxSamples\": %s,
           \"featureCount\": %d,
           \"streamingWindowSize\": %d,
           \"features\": [%s],
           \"percentVariation\": %d,
           \"samplesToBuffer\": %s
        }",
            self$anomalyHistoryWindow$toJSON(), self$learningRateNumerator$toJSON(),
            self$learningRateDenominator$toJSON(), self$learningMaxClusters$toJSON(),
            self$learningMaxSamples$toJSON(), self$featureCount, self$streamingWindowSize,
            lapply(self$features, function(x) paste(x$toJSON(), sep = ",")), self$percentVariation,
            self$samplesToBuffer$toJSON())
    }, fromJSONString = function(GetConfigResponseJson) {
        GetConfigResponseObject <- jsonlite::fromJSON(GetConfigResponseJson)
        BigDecimalObject <- BigDecimal$new()
        self$anomalyHistoryWindow <- BigDecimalObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$anomalyHistoryWindow,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningRateNumerator <- BigDecimalObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningRateNumerator,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningRateDenominator <- BigDecimalObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningRateDenominator,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningMaxClusters <- BigDecimalObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningMaxClusters,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningMaxSamples <- BigDecimalObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$learningMaxSamples,
            auto_unbox = TRUE))
        self$featureCount <- GetConfigResponseObject$featureCount
        self$streamingWindowSize <- GetConfigResponseObject$streamingWindowSize
        self$features <- lapply(GetConfigResponseObject$features, function(x) FeatureConfig$new()$fromJSON(jsonlite::toJSON(x,
            auto_unbox = TRUE)))
        self$percentVariation <- GetConfigResponseObject$percentVariation
        BigDecimalObject <- BigDecimal$new()
        self$samplesToBuffer <- BigDecimalObject$fromJSON(jsonlite::toJSON(GetConfigResponseObject$samplesToBuffer,
            auto_unbox = TRUE))
    }))
