# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MAP Class
#'
#' @field VersionNumber 
#' @field m_AutotuneRange 
#' @field m_AutotunePV 
#' @field m_AutotuneByFeatures 
#' @field m_MaxClusters 
#' @field m_FeaturesToTuneArray 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MAP <- R6::R6Class("MAP", public = list(VersionNumber = NULL, m_AutotuneRange = NULL,
    m_AutotunePV = NULL, m_AutotuneByFeatures = NULL, m_MaxClusters = NULL, m_FeaturesToTuneArray = NULL,
    initialize = function(VersionNumber, m_AutotuneRange, m_AutotunePV, m_AutotuneByFeatures,
        m_MaxClusters, m_FeaturesToTuneArray) {
        if (!missing(VersionNumber)) {
            stopifnot(R6::is.R6(VersionNumber))
            self$VersionNumber <- VersionNumber
        }
        if (!missing(m_AutotuneRange)) {
            self$m_AutotuneRange <- m_AutotuneRange
        }
        if (!missing(m_AutotunePV)) {
            self$m_AutotunePV <- m_AutotunePV
        }
        if (!missing(m_AutotuneByFeatures)) {
            self$m_AutotuneByFeatures <- m_AutotuneByFeatures
        }
        if (!missing(m_MaxClusters)) {
            stopifnot(is.numeric(m_MaxClusters), length(m_MaxClusters) == 1)
            self$m_MaxClusters <- m_MaxClusters
        }
        if (!missing(m_FeaturesToTuneArray)) {
            stopifnot(is.list(m_FeaturesToTuneArray), length(m_FeaturesToTuneArray) !=
                0)
            lapply(m_FeaturesToTuneArray, function(x) stopifnot(is.character(x)))
            self$m_FeaturesToTuneArray <- m_FeaturesToTuneArray
        }
    }, toJSON = function() {
        MAPObject <- list()
        if (!is.null(self$VersionNumber)) {
            MAPObject[["VersionNumber"]] <- self$VersionNumber$toJSON()
        }
        if (!is.null(self$m_AutotuneRange)) {
            MAPObject[["m_AutotuneRange"]] <- self$m_AutotuneRange
        }
        if (!is.null(self$m_AutotunePV)) {
            MAPObject[["m_AutotunePV"]] <- self$m_AutotunePV
        }
        if (!is.null(self$m_AutotuneByFeatures)) {
            MAPObject[["m_AutotuneByFeatures"]] <- self$m_AutotuneByFeatures
        }
        if (!is.null(self$m_MaxClusters)) {
            MAPObject[["m_MaxClusters"]] <- self$m_MaxClusters
        }
        if (!is.null(self$m_FeaturesToTuneArray)) {
            MAPObject[["m_FeaturesToTuneArray"]] <- self$m_FeaturesToTuneArray
        }

        MAPObject
    }, fromJSON = function(MAPJson) {
        MAPObject <- jsonlite::fromJSON(MAPJson)
        if (!is.null(MAPObject$VersionNumber)) {
            VersionNumberObject <- VersionNumber$new()
            VersionNumberObject$fromJSON(jsonlite::toJSON(MAPObject$VersionNumber,
                auto_unbox = TRUE))
            self$VersionNumber <- VersionNumberObject
        }
        if (!is.null(MAPObject$m_AutotuneRange)) {
            self$m_AutotuneRange <- MAPObject$m_AutotuneRange
        }
        if (!is.null(MAPObject$m_AutotunePV)) {
            self$m_AutotunePV <- MAPObject$m_AutotunePV
        }
        if (!is.null(MAPObject$m_AutotuneByFeatures)) {
            self$m_AutotuneByFeatures <- MAPObject$m_AutotuneByFeatures
        }
        if (!is.null(MAPObject$m_MaxClusters)) {
            self$m_MaxClusters <- MAPObject$m_MaxClusters
        }
        if (!is.null(MAPObject$m_FeaturesToTuneArray)) {
            self$m_FeaturesToTuneArray <- MAPObject$m_FeaturesToTuneArray
        }
    }, toJSONString = function() {
        sprintf("{
           \"VersionNumber\": %s,
           \"m_AutotuneRange\": %s,
           \"m_AutotunePV\": %s,
           \"m_AutotuneByFeatures\": %s,
           \"m_MaxClusters\": %d,
           \"m_FeaturesToTuneArray\": [%s]
        }",
            self$VersionNumber$toJSON(), self$m_AutotuneRange, self$m_AutotunePV,
            self$m_AutotuneByFeatures, self$m_MaxClusters, lapply(self$m_FeaturesToTuneArray,
                function(x) paste(paste0("\"", x, "\""), sep = ",")))
    }, fromJSONString = function(MAPJson) {
        MAPObject <- jsonlite::fromJSON(MAPJson)
        VersionNumberObject <- VersionNumber$new()
        self$VersionNumber <- VersionNumberObject$fromJSON(jsonlite::toJSON(MAPObject$VersionNumber,
            auto_unbox = TRUE))
        self$m_AutotuneRange <- MAPObject$m_AutotuneRange
        self$m_AutotunePV <- MAPObject$m_AutotunePV
        self$m_AutotuneByFeatures <- MAPObject$m_AutotuneByFeatures
        self$m_MaxClusters <- MAPObject$m_MaxClusters
        self$m_FeaturesToTuneArray <- MAPObject$m_FeaturesToTuneArray
    }))
