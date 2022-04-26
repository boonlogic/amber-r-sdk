# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MStreamingParameters Class
#'
#' @field VersionNumber 
#' @field m_EnableAutotuning 
#' @field m_SamplesToBufferForAutotuning 
#' @field m_GraduationAtRateNumerator 
#' @field m_GraduationAtRateDenominator 
#' @field m_GraduateAtMaxClusters 
#' @field m_GraduateAtMaxSamples 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MStreamingParameters <- R6::R6Class("MStreamingParameters", public = list(VersionNumber = NULL,
    m_EnableAutotuning = NULL, m_SamplesToBufferForAutotuning = NULL, m_GraduationAtRateNumerator = NULL,
    m_GraduationAtRateDenominator = NULL, m_GraduateAtMaxClusters = NULL, m_GraduateAtMaxSamples = NULL,
    initialize = function(VersionNumber, m_EnableAutotuning, m_SamplesToBufferForAutotuning,
        m_GraduationAtRateNumerator, m_GraduationAtRateDenominator, m_GraduateAtMaxClusters,
        m_GraduateAtMaxSamples) {
        if (!missing(VersionNumber)) {
            stopifnot(R6::is.R6(VersionNumber))
            self$VersionNumber <- VersionNumber
        }
        if (!missing(m_EnableAutotuning)) {
            self$m_EnableAutotuning <- m_EnableAutotuning
        }
        if (!missing(m_SamplesToBufferForAutotuning)) {
            stopifnot(is.numeric(m_SamplesToBufferForAutotuning), length(m_SamplesToBufferForAutotuning) ==
                1)
            self$m_SamplesToBufferForAutotuning <- m_SamplesToBufferForAutotuning
        }
        if (!missing(m_GraduationAtRateNumerator)) {
            stopifnot(is.numeric(m_GraduationAtRateNumerator), length(m_GraduationAtRateNumerator) ==
                1)
            self$m_GraduationAtRateNumerator <- m_GraduationAtRateNumerator
        }
        if (!missing(m_GraduationAtRateDenominator)) {
            stopifnot(is.numeric(m_GraduationAtRateDenominator), length(m_GraduationAtRateDenominator) ==
                1)
            self$m_GraduationAtRateDenominator <- m_GraduationAtRateDenominator
        }
        if (!missing(m_GraduateAtMaxClusters)) {
            stopifnot(is.numeric(m_GraduateAtMaxClusters), length(m_GraduateAtMaxClusters) ==
                1)
            self$m_GraduateAtMaxClusters <- m_GraduateAtMaxClusters
        }
        if (!missing(m_GraduateAtMaxSamples)) {
            stopifnot(is.numeric(m_GraduateAtMaxSamples), length(m_GraduateAtMaxSamples) ==
                1)
            self$m_GraduateAtMaxSamples <- m_GraduateAtMaxSamples
        }
    }, toJSON = function() {
        MStreamingParametersObject <- list()
        if (!is.null(self$VersionNumber)) {
            MStreamingParametersObject[["VersionNumber"]] <- self$VersionNumber$toJSON()
        }
        if (!is.null(self$m_EnableAutotuning)) {
            MStreamingParametersObject[["m_EnableAutotuning"]] <- self$m_EnableAutotuning
        }
        if (!is.null(self$m_SamplesToBufferForAutotuning)) {
            MStreamingParametersObject[["m_SamplesToBufferForAutotuning"]] <- self$m_SamplesToBufferForAutotuning
        }
        if (!is.null(self$m_GraduationAtRateNumerator)) {
            MStreamingParametersObject[["m_GraduationAtRateNumerator"]] <- self$m_GraduationAtRateNumerator
        }
        if (!is.null(self$m_GraduationAtRateDenominator)) {
            MStreamingParametersObject[["m_GraduationAtRateDenominator"]] <- self$m_GraduationAtRateDenominator
        }
        if (!is.null(self$m_GraduateAtMaxClusters)) {
            MStreamingParametersObject[["m_GraduateAtMaxClusters"]] <- self$m_GraduateAtMaxClusters
        }
        if (!is.null(self$m_GraduateAtMaxSamples)) {
            MStreamingParametersObject[["m_GraduateAtMaxSamples"]] <- self$m_GraduateAtMaxSamples
        }

        MStreamingParametersObject
    }, fromJSON = function(MStreamingParametersJson) {
        MStreamingParametersObject <- jsonlite::fromJSON(MStreamingParametersJson)
        if (!is.null(MStreamingParametersObject$VersionNumber)) {
            VersionNumberObject <- VersionNumber$new()
            VersionNumberObject$fromJSON(jsonlite::toJSON(MStreamingParametersObject$VersionNumber,
                auto_unbox = TRUE))
            self$VersionNumber <- VersionNumberObject
        }
        if (!is.null(MStreamingParametersObject$m_EnableAutotuning)) {
            self$m_EnableAutotuning <- MStreamingParametersObject$m_EnableAutotuning
        }
        if (!is.null(MStreamingParametersObject$m_SamplesToBufferForAutotuning)) {
            self$m_SamplesToBufferForAutotuning <- MStreamingParametersObject$m_SamplesToBufferForAutotuning
        }
        if (!is.null(MStreamingParametersObject$m_GraduationAtRateNumerator)) {
            self$m_GraduationAtRateNumerator <- MStreamingParametersObject$m_GraduationAtRateNumerator
        }
        if (!is.null(MStreamingParametersObject$m_GraduationAtRateDenominator)) {
            self$m_GraduationAtRateDenominator <- MStreamingParametersObject$m_GraduationAtRateDenominator
        }
        if (!is.null(MStreamingParametersObject$m_GraduateAtMaxClusters)) {
            self$m_GraduateAtMaxClusters <- MStreamingParametersObject$m_GraduateAtMaxClusters
        }
        if (!is.null(MStreamingParametersObject$m_GraduateAtMaxSamples)) {
            self$m_GraduateAtMaxSamples <- MStreamingParametersObject$m_GraduateAtMaxSamples
        }
    }, toJSONString = function() {
        sprintf("{
           \"VersionNumber\": %s,
           \"m_EnableAutotuning\": %s,
           \"m_SamplesToBufferForAutotuning\": %d,
           \"m_GraduationAtRateNumerator\": %d,
           \"m_GraduationAtRateDenominator\": %d,
           \"m_GraduateAtMaxClusters\": %d,
           \"m_GraduateAtMaxSamples\": %d
        }",
            self$VersionNumber$toJSON(), self$m_EnableAutotuning, self$m_SamplesToBufferForAutotuning,
            self$m_GraduationAtRateNumerator, self$m_GraduationAtRateDenominator,
            self$m_GraduateAtMaxClusters, self$m_GraduateAtMaxSamples)
    }, fromJSONString = function(MStreamingParametersJson) {
        MStreamingParametersObject <- jsonlite::fromJSON(MStreamingParametersJson)
        VersionNumberObject <- VersionNumber$new()
        self$VersionNumber <- VersionNumberObject$fromJSON(jsonlite::toJSON(MStreamingParametersObject$VersionNumber,
            auto_unbox = TRUE))
        self$m_EnableAutotuning <- MStreamingParametersObject$m_EnableAutotuning
        self$m_SamplesToBufferForAutotuning <- MStreamingParametersObject$m_SamplesToBufferForAutotuning
        self$m_GraduationAtRateNumerator <- MStreamingParametersObject$m_GraduationAtRateNumerator
        self$m_GraduationAtRateDenominator <- MStreamingParametersObject$m_GraduationAtRateDenominator
        self$m_GraduateAtMaxClusters <- MStreamingParametersObject$m_GraduateAtMaxClusters
        self$m_GraduateAtMaxSamples <- MStreamingParametersObject$m_GraduateAtMaxSamples
    }))
