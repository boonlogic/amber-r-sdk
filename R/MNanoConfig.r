# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MNanoConfig Class
#'
#' @field VersionNumber 
#' @field m_NumericFormat 
#' @field m_Accuracy 
#' @field m_PercentVariation 
#' @field NumOfFeatures 
#' @field Features 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MNanoConfig <- R6::R6Class("MNanoConfig", public = list(VersionNumber = NULL, m_NumericFormat = NULL,
    m_Accuracy = NULL, m_PercentVariation = NULL, NumOfFeatures = NULL, Features = NULL,
    initialize = function(VersionNumber, m_NumericFormat, m_Accuracy, m_PercentVariation,
        NumOfFeatures, Features) {
        if (!missing(VersionNumber)) {
            stopifnot(R6::is.R6(VersionNumber))
            self$VersionNumber <- VersionNumber
        }
        if (!missing(m_NumericFormat)) {
            stopifnot(is.numeric(m_NumericFormat), length(m_NumericFormat) == 1)
            self$m_NumericFormat <- m_NumericFormat
        }
        if (!missing(m_Accuracy)) {
            stopifnot(is.numeric(m_Accuracy), length(m_Accuracy) == 1)
            self$m_Accuracy <- m_Accuracy
        }
        if (!missing(m_PercentVariation)) {
            stopifnot(is.numeric(m_PercentVariation), length(m_PercentVariation) ==
                1)
            self$m_PercentVariation <- m_PercentVariation
        }
        if (!missing(NumOfFeatures)) {
            stopifnot(is.numeric(NumOfFeatures), length(NumOfFeatures) == 1)
            self$NumOfFeatures <- NumOfFeatures
        }
        if (!missing(Features)) {
            stopifnot(is.list(Features), length(Features) != 0)
            lapply(Features, function(x) stopifnot(R6::is.R6(x)))
            self$Features <- Features
        }
    }, toJSON = function() {
        MNanoConfigObject <- list()
        if (!is.null(self$VersionNumber)) {
            MNanoConfigObject[["VersionNumber"]] <- self$VersionNumber$toJSON()
        }
        if (!is.null(self$m_NumericFormat)) {
            MNanoConfigObject[["m_NumericFormat"]] <- self$m_NumericFormat
        }
        if (!is.null(self$m_Accuracy)) {
            MNanoConfigObject[["m_Accuracy"]] <- self$m_Accuracy
        }
        if (!is.null(self$m_PercentVariation)) {
            MNanoConfigObject[["m_PercentVariation"]] <- self$m_PercentVariation
        }
        if (!is.null(self$NumOfFeatures)) {
            MNanoConfigObject[["NumOfFeatures"]] <- self$NumOfFeatures
        }
        if (!is.null(self$Features)) {
            MNanoConfigObject[["Features"]] <- lapply(self$Features, function(x) x$toJSON())
        }

        MNanoConfigObject
    }, fromJSON = function(MNanoConfigJson) {
        MNanoConfigObject <- jsonlite::fromJSON(MNanoConfigJson)
        if (!is.null(MNanoConfigObject$VersionNumber)) {
            VersionNumberObject <- VersionNumber$new()
            VersionNumberObject$fromJSON(jsonlite::toJSON(MNanoConfigObject$VersionNumber,
                auto_unbox = TRUE))
            self$VersionNumber <- VersionNumberObject
        }
        if (!is.null(MNanoConfigObject$m_NumericFormat)) {
            self$m_NumericFormat <- MNanoConfigObject$m_NumericFormat
        }
        if (!is.null(MNanoConfigObject$m_Accuracy)) {
            self$m_Accuracy <- MNanoConfigObject$m_Accuracy
        }
        if (!is.null(MNanoConfigObject$m_PercentVariation)) {
            self$m_PercentVariation <- MNanoConfigObject$m_PercentVariation
        }
        if (!is.null(MNanoConfigObject$NumOfFeatures)) {
            self$NumOfFeatures <- MNanoConfigObject$NumOfFeatures
        }
        if (!is.null(MNanoConfigObject$Features)) {
            self$Features <- lapply(MNanoConfigObject$Features, function(x) {
                FeaturesObject <- FeatureConfig$new()
                FeaturesObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
                FeaturesObject
            })
        }
    }, toJSONString = function() {
        sprintf("{
           \"VersionNumber\": %s,
           \"m_NumericFormat\": %d,
           \"m_Accuracy\": %d,
           \"m_PercentVariation\": %d,
           \"NumOfFeatures\": %d,
           \"Features\": [%s]
        }",
            self$VersionNumber$toJSON(), self$m_NumericFormat, self$m_Accuracy, self$m_PercentVariation,
            self$NumOfFeatures, lapply(self$Features, function(x) paste(x$toJSON(),
                sep = ",")))
    }, fromJSONString = function(MNanoConfigJson) {
        MNanoConfigObject <- jsonlite::fromJSON(MNanoConfigJson)
        VersionNumberObject <- VersionNumber$new()
        self$VersionNumber <- VersionNumberObject$fromJSON(jsonlite::toJSON(MNanoConfigObject$VersionNumber,
            auto_unbox = TRUE))
        self$m_NumericFormat <- MNanoConfigObject$m_NumericFormat
        self$m_Accuracy <- MNanoConfigObject$m_Accuracy
        self$m_PercentVariation <- MNanoConfigObject$m_PercentVariation
        self$NumOfFeatures <- MNanoConfigObject$NumOfFeatures
        self$Features <- lapply(MNanoConfigObject$Features, function(x) FeatureConfig$new()$fromJSON(jsonlite::toJSON(x,
            auto_unbox = TRUE)))
    }))