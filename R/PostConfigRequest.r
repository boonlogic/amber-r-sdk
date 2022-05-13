# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostConfigRequest Class
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
PostConfigRequest <- R6::R6Class(
  'PostConfigRequest',
  public = list(
    `anomalyHistoryWindow` = NULL,
    `learningRateNumerator` = NULL,
    `learningRateDenominator` = NULL,
    `learningMaxClusters` = NULL,
    `learningMaxSamples` = NULL,
    `featureCount` = NULL,
    `streamingWindowSize` = NULL,
    `features` = NULL,
    `samplesToBuffer` = NULL,
    initialize = function(`anomalyHistoryWindow`, `learningRateNumerator`, `learningRateDenominator`, `learningMaxClusters`, `learningMaxSamples`, `featureCount`, `streamingWindowSize`, `features`, `samplesToBuffer`){
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
      if (!missing(`featureCount`)) {
        stopifnot(is.numeric(`featureCount`), length(`featureCount`) == 1)
        self$`featureCount` <- `featureCount`
      }
      if (!missing(`streamingWindowSize`)) {
        stopifnot(is.numeric(`streamingWindowSize`), length(`streamingWindowSize`) == 1)
        self$`streamingWindowSize` <- `streamingWindowSize`
      }
      if (!missing(`features`)) {
        stopifnot(is.list(`features`), length(`features`) != 0)
        lapply(`features`, function(x) stopifnot(R6::is.R6(x)))
        self$`features` <- `features`
      }
      if (!missing(`samplesToBuffer`)) {
        stopifnot(is.numeric(`samplesToBuffer`), length(`samplesToBuffer`) == 1)
        stopifnot(R6::is.R6(`samplesToBuffer`))
        self$`samplesToBuffer` <- `samplesToBuffer`
      }
    },
    toJSON = function() {
      PostConfigRequestObject <- list()
      if (!is.null(self$`anomalyHistoryWindow`)) {
        PostConfigRequestObject[['anomalyHistoryWindow']] <- self$`anomalyHistoryWindow`
      }
      if (!is.null(self$`learningRateNumerator`)) {
        PostConfigRequestObject[['learningRateNumerator']] <- self$`learningRateNumerator`
      }
      if (!is.null(self$`learningRateDenominator`)) {
        PostConfigRequestObject[['learningRateDenominator']] <- self$`learningRateDenominator`
      }
      if (!is.null(self$`learningMaxClusters`)) {
        PostConfigRequestObject[['learningMaxClusters']] <- self$`learningMaxClusters`
      }
      if (!is.null(self$`learningMaxSamples`)) {
        PostConfigRequestObject[['learningMaxSamples']] <- self$`learningMaxSamples`
      }
      if (!is.null(self$`featureCount`)) {
        PostConfigRequestObject[['featureCount']] <- self$`featureCount`
      }
      if (!is.null(self$`streamingWindowSize`)) {
        PostConfigRequestObject[['streamingWindowSize']] <- self$`streamingWindowSize`
      }
      if (!is.null(self$`features`)) {
        PostConfigRequestObject[['features']] <- lapply(self$`features`, function(x) x$toJSON())
      }
      if (!is.null(self$`samplesToBuffer`)) {
        PostConfigRequestObject[['samplesToBuffer']] <- self$`samplesToBuffer`
      }

      PostConfigRequestObject
    },
    fromJSON = function(PostConfigRequestJson) {
      PostConfigRequestObject <- jsonlite::fromJSON(PostConfigRequestJson, simplifyVector = FALSE)
      if (!is.null(PostConfigRequestObject$`anomalyHistoryWindow`)) {
        self$`anomalyHistoryWindow` <- PostConfigRequestObject$anomalyHistoryWindow
      }
      if (!is.null(PostConfigRequestObject$`learningRateNumerator`)) {
        self$`learningRateNumerator` <- PostConfigRequestObject$learningRateNumerator
      }
      if (!is.null(PostConfigRequestObject$`learningRateDenominator`)) {
        self$`learningRateDenominator` <- PostConfigRequestObject$learningRateDenominator
      }
      if (!is.null(PostConfigRequestObject$`learningMaxClusters`)) {
        self$`learningMaxClusters` <- PostConfigRequestObject$learningMaxClusters
      }
      if (!is.null(PostConfigRequestObject$`learningMaxSamples`)) {
        self$`learningMaxSamples` <- PostConfigRequestObject$learningMaxSamples
      }
      if (!is.null(PostConfigRequestObject$`featureCount`)) {
        self$`featureCount` <- PostConfigRequestObject$`featureCount`
      }
      if (!is.null(PostConfigRequestObject$`streamingWindowSize`)) {
        self$`streamingWindowSize` <- PostConfigRequestObject$`streamingWindowSize`
      }
      if (!is.null(PostConfigRequestObject$`features`)) {
        self$`features` <- lapply(PostConfigRequestObject$`features`, function(x) {
          featuresObject <- FeatureConfig$new()
          featuresObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          featuresObject
        })
      }
      if (!is.null(PostConfigRequestObject$`samplesToBuffer`)) {
        self$`samplesToBuffer` <- PostConfigRequestObject$samplesToBuffer
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "anomalyHistoryWindow": %s,
           "learningRateNumerator": %s,
           "learningRateDenominator": %s,
           "learningMaxClusters": %s,
           "learningMaxSamples": %s,
           "featureCount": %d,
           "streamingWindowSize": %d,
           "features": [%s],
           "samplesToBuffer": %s
        }',
        self$`anomalyHistoryWindow`,
        self$`learningRateNumerator`,
        self$`learningRateDenominator`,
        self$`learningMaxClusters`,
        self$`learningMaxSamples`,
        self$`featureCount`,
        self$`streamingWindowSize`,
        lapply(self$`features`, function(x) paste(x$toJSON(), sep=",")),
        self$`samplesToBuffer`
      )
    },
    fromJSONString = function(PostConfigRequestJson) {
      PostConfigRequestObject <- jsonlite::fromJSON(PostConfigRequestJson, simplifyVector = FALSE)
      self$`anomalyHistoryWindow` <- PostConfigRequestObject$anomalyHistoryWindow
      self$`learningRateNumerator` <- PostConfigRequestObject$learningRateNumerator
      self$`learningRateDenominator` <- PostConfigRequestObject$learningRateDenominator
      self$`learningMaxClusters` <- PostConfigRequestObject$learningMaxClusters
      self$`learningMaxSamples` <- PostConfigRequestObject$learningMaxSamples
      self$`featureCount` <- PostConfigRequestObject$`featureCount`
      self$`streamingWindowSize` <- PostConfigRequestObject$`streamingWindowSize`
      self$`features` <- lapply(PostConfigRequestObject$`features`, function(x) FeatureConfig$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`samplesToBuffer` <- PostConfigRequestObject$samplesToBuffer
    }
  )
)

