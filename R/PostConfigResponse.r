# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostConfigResponse Class
#'
#' @field anomalyHistoryWindow 
#' @field learningRateNumerator 
#' @field learningRateDenominator 
#' @field learningMaxClusters 
#' @field learningMaxSamples 
#' @field featureCount 
#' @field streamingWindowSize 
#' @field features 
#' @field samplesToBuffer 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostConfigResponse <- R6::R6Class("PostConfigResponse", public = list(anomalyHistoryWindow = NULL,
    learningRateNumerator = NULL, learningRateDenominator = NULL, learningMaxClusters = NULL,
    learningMaxSamples = NULL, featureCount = NULL, streamingWindowSize = NULL, features = NULL,
    samplesToBuffer = NULL, initialize = function(anomalyHistoryWindow, learningRateNumerator,
        learningRateDenominator, learningMaxClusters, learningMaxSamples, featureCount,
        streamingWindowSize, features, samplesToBuffer) {
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
        if (!missing(samplesToBuffer)) {
            stopifnot(is.numeric(samplesToBuffer), length(samplesToBuffer) == 1)
            stopifnot(R6::is.R6(samplesToBuffer))
            self$samplesToBuffer <- samplesToBuffer
        }
    }, toJSON = function() {
        PostConfigResponseObject <- list()
        if (!is.null(self$anomalyHistoryWindow)) {
            PostConfigResponseObject[["anomalyHistoryWindow"]] <- self$anomalyHistoryWindow$toJSON()
        }
        if (!is.null(self$learningRateNumerator)) {
            PostConfigResponseObject[["learningRateNumerator"]] <- self$learningRateNumerator$toJSON()
        }
        if (!is.null(self$learningRateDenominator)) {
            PostConfigResponseObject[["learningRateDenominator"]] <- self$learningRateDenominator$toJSON()
        }
        if (!is.null(self$learningMaxClusters)) {
            PostConfigResponseObject[["learningMaxClusters"]] <- self$learningMaxClusters$toJSON()
        }
        if (!is.null(self$learningMaxSamples)) {
            PostConfigResponseObject[["learningMaxSamples"]] <- self$learningMaxSamples$toJSON()
        }
        if (!is.null(self$featureCount)) {
            PostConfigResponseObject[["featureCount"]] <- self$featureCount
        }
        if (!is.null(self$streamingWindowSize)) {
            PostConfigResponseObject[["streamingWindowSize"]] <- self$streamingWindowSize
        }
        if (!is.null(self$features)) {
            PostConfigResponseObject[["features"]] <- lapply(self$features, function(x) x$toJSON())
        }
        if (!is.null(self$samplesToBuffer)) {
            PostConfigResponseObject[["samplesToBuffer"]] <- self$samplesToBuffer$toJSON()
        }

        PostConfigResponseObject
    }, fromJSON = function(PostConfigResponseJson) {
        PostConfigResponseObject <- jsonlite::fromJSON(PostConfigResponseJson)
        if (!is.null(PostConfigResponseObject$anomalyHistoryWindow)) {
            anomalyHistoryWindowObject <- BigDecimal$new()
            anomalyHistoryWindowObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$anomalyHistoryWindow,
                auto_unbox = TRUE))
            self$anomalyHistoryWindow <- anomalyHistoryWindowObject
        }
        if (!is.null(PostConfigResponseObject$learningRateNumerator)) {
            learningRateNumeratorObject <- BigDecimal$new()
            learningRateNumeratorObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningRateNumerator,
                auto_unbox = TRUE))
            self$learningRateNumerator <- learningRateNumeratorObject
        }
        if (!is.null(PostConfigResponseObject$learningRateDenominator)) {
            learningRateDenominatorObject <- BigDecimal$new()
            learningRateDenominatorObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningRateDenominator,
                auto_unbox = TRUE))
            self$learningRateDenominator <- learningRateDenominatorObject
        }
        if (!is.null(PostConfigResponseObject$learningMaxClusters)) {
            learningMaxClustersObject <- BigDecimal$new()
            learningMaxClustersObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningMaxClusters,
                auto_unbox = TRUE))
            self$learningMaxClusters <- learningMaxClustersObject
        }
        if (!is.null(PostConfigResponseObject$learningMaxSamples)) {
            learningMaxSamplesObject <- BigDecimal$new()
            learningMaxSamplesObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningMaxSamples,
                auto_unbox = TRUE))
            self$learningMaxSamples <- learningMaxSamplesObject
        }
        if (!is.null(PostConfigResponseObject$featureCount)) {
            self$featureCount <- PostConfigResponseObject$featureCount
        }
        if (!is.null(PostConfigResponseObject$streamingWindowSize)) {
            self$streamingWindowSize <- PostConfigResponseObject$streamingWindowSize
        }
        if (!is.null(PostConfigResponseObject$features)) {
            self$features <- lapply(PostConfigResponseObject$features, function(x) {
                featuresObject <- FeatureConfig$new()
                featuresObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
                featuresObject
            })
        }
        if (!is.null(PostConfigResponseObject$samplesToBuffer)) {
            samplesToBufferObject <- BigDecimal$new()
            samplesToBufferObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$samplesToBuffer,
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
           \"samplesToBuffer\": %s
        }",
            self$anomalyHistoryWindow$toJSON(), self$learningRateNumerator$toJSON(),
            self$learningRateDenominator$toJSON(), self$learningMaxClusters$toJSON(),
            self$learningMaxSamples$toJSON(), self$featureCount, self$streamingWindowSize,
            lapply(self$features, function(x) paste(x$toJSON(), sep = ",")), self$samplesToBuffer$toJSON())
    }, fromJSONString = function(PostConfigResponseJson) {
        PostConfigResponseObject <- jsonlite::fromJSON(PostConfigResponseJson)
        BigDecimalObject <- BigDecimal$new()
        self$anomalyHistoryWindow <- BigDecimalObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$anomalyHistoryWindow,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningRateNumerator <- BigDecimalObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningRateNumerator,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningRateDenominator <- BigDecimalObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningRateDenominator,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningMaxClusters <- BigDecimalObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningMaxClusters,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$learningMaxSamples <- BigDecimalObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$learningMaxSamples,
            auto_unbox = TRUE))
        self$featureCount <- PostConfigResponseObject$featureCount
        self$streamingWindowSize <- PostConfigResponseObject$streamingWindowSize
        self$features <- lapply(PostConfigResponseObject$features, function(x) FeatureConfig$new()$fromJSON(jsonlite::toJSON(x,
            auto_unbox = TRUE)))
        BigDecimalObject <- BigDecimal$new()
        self$samplesToBuffer <- BigDecimalObject$fromJSON(jsonlite::toJSON(PostConfigResponseObject$samplesToBuffer,
            auto_unbox = TRUE))
    }))
