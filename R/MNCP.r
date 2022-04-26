# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MNCP Class
#'
#' @field VersionNumber 
#' @field NumOfFeatures 
#' @field m_NumericFormat 
#' @field m_PercentVariation 
#' @field m_Accuracy 
#' @field m_StreamingWindowSize 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MNCP <- R6::R6Class("MNCP", public = list(VersionNumber = NULL, NumOfFeatures = NULL,
    m_NumericFormat = NULL, m_PercentVariation = NULL, m_Accuracy = NULL, m_StreamingWindowSize = NULL,
    initialize = function(VersionNumber, NumOfFeatures, m_NumericFormat, m_PercentVariation,
        m_Accuracy, m_StreamingWindowSize) {
        if (!missing(VersionNumber)) {
            stopifnot(R6::is.R6(VersionNumber))
            self$VersionNumber <- VersionNumber
        }
        if (!missing(NumOfFeatures)) {
            stopifnot(is.numeric(NumOfFeatures), length(NumOfFeatures) == 1)
            self$NumOfFeatures <- NumOfFeatures
        }
        if (!missing(m_NumericFormat)) {
            stopifnot(is.numeric(m_NumericFormat), length(m_NumericFormat) == 1)
            self$m_NumericFormat <- m_NumericFormat
        }
        if (!missing(m_PercentVariation)) {
            stopifnot(is.numeric(m_PercentVariation), length(m_PercentVariation) ==
                1)
            self$m_PercentVariation <- m_PercentVariation
        }
        if (!missing(m_Accuracy)) {
            stopifnot(is.numeric(m_Accuracy), length(m_Accuracy) == 1)
            self$m_Accuracy <- m_Accuracy
        }
        if (!missing(m_StreamingWindowSize)) {
            stopifnot(is.numeric(m_StreamingWindowSize), length(m_StreamingWindowSize) ==
                1)
            self$m_StreamingWindowSize <- m_StreamingWindowSize
        }
    }, toJSON = function() {
        MNCPObject <- list()
        if (!is.null(self$VersionNumber)) {
            MNCPObject[["VersionNumber"]] <- self$VersionNumber$toJSON()
        }
        if (!is.null(self$NumOfFeatures)) {
            MNCPObject[["NumOfFeatures"]] <- self$NumOfFeatures
        }
        if (!is.null(self$m_NumericFormat)) {
            MNCPObject[["m_NumericFormat"]] <- self$m_NumericFormat
        }
        if (!is.null(self$m_PercentVariation)) {
            MNCPObject[["m_PercentVariation"]] <- self$m_PercentVariation
        }
        if (!is.null(self$m_Accuracy)) {
            MNCPObject[["m_Accuracy"]] <- self$m_Accuracy
        }
        if (!is.null(self$m_StreamingWindowSize)) {
            MNCPObject[["m_StreamingWindowSize"]] <- self$m_StreamingWindowSize
        }

        MNCPObject
    }, fromJSON = function(MNCPJson) {
        MNCPObject <- jsonlite::fromJSON(MNCPJson)
        if (!is.null(MNCPObject$VersionNumber)) {
            VersionNumberObject <- VersionNumber$new()
            VersionNumberObject$fromJSON(jsonlite::toJSON(MNCPObject$VersionNumber,
                auto_unbox = TRUE))
            self$VersionNumber <- VersionNumberObject
        }
        if (!is.null(MNCPObject$NumOfFeatures)) {
            self$NumOfFeatures <- MNCPObject$NumOfFeatures
        }
        if (!is.null(MNCPObject$m_NumericFormat)) {
            self$m_NumericFormat <- MNCPObject$m_NumericFormat
        }
        if (!is.null(MNCPObject$m_PercentVariation)) {
            self$m_PercentVariation <- MNCPObject$m_PercentVariation
        }
        if (!is.null(MNCPObject$m_Accuracy)) {
            self$m_Accuracy <- MNCPObject$m_Accuracy
        }
        if (!is.null(MNCPObject$m_StreamingWindowSize)) {
            self$m_StreamingWindowSize <- MNCPObject$m_StreamingWindowSize
        }
    }, toJSONString = function() {
        sprintf("{
           \"VersionNumber\": %s,
           \"NumOfFeatures\": %d,
           \"m_NumericFormat\": %d,
           \"m_PercentVariation\": %d,
           \"m_Accuracy\": %d,
           \"m_StreamingWindowSize\": %d
        }",
            self$VersionNumber$toJSON(), self$NumOfFeatures, self$m_NumericFormat,
            self$m_PercentVariation, self$m_Accuracy, self$m_StreamingWindowSize)
    }, fromJSONString = function(MNCPJson) {
        MNCPObject <- jsonlite::fromJSON(MNCPJson)
        VersionNumberObject <- VersionNumber$new()
        self$VersionNumber <- VersionNumberObject$fromJSON(jsonlite::toJSON(MNCPObject$VersionNumber,
            auto_unbox = TRUE))
        self$NumOfFeatures <- MNCPObject$NumOfFeatures
        self$m_NumericFormat <- MNCPObject$m_NumericFormat
        self$m_PercentVariation <- MNCPObject$m_PercentVariation
        self$m_Accuracy <- MNCPObject$m_Accuracy
        self$m_StreamingWindowSize <- MNCPObject$m_StreamingWindowSize
    }))