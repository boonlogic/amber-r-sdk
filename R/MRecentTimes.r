# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MRecentTimes Class
#'
#' @field VersionNumber 
#' @field m_Values 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MRecentTimes <- R6::R6Class("MRecentTimes", public = list(VersionNumber = NULL, m_Values = NULL,
    initialize = function(VersionNumber, m_Values) {
        if (!missing(VersionNumber)) {
            stopifnot(R6::is.R6(VersionNumber))
            self$VersionNumber <- VersionNumber
        }
        if (!missing(m_Values)) {
            stopifnot(is.list(m_Values), length(m_Values) != 0)
            lapply(m_Values, function(x) stopifnot(is.character(x)))
            self$m_Values <- m_Values
        }
    }, toJSON = function() {
        MRecentTimesObject <- list()
        if (!is.null(self$VersionNumber)) {
            MRecentTimesObject[["VersionNumber"]] <- self$VersionNumber$toJSON()
        }
        if (!is.null(self$m_Values)) {
            MRecentTimesObject[["m_Values"]] <- self$m_Values
        }

        MRecentTimesObject
    }, fromJSON = function(MRecentTimesJson) {
        MRecentTimesObject <- jsonlite::fromJSON(MRecentTimesJson)
        if (!is.null(MRecentTimesObject$VersionNumber)) {
            VersionNumberObject <- VersionNumber$new()
            VersionNumberObject$fromJSON(jsonlite::toJSON(MRecentTimesObject$VersionNumber,
                auto_unbox = TRUE))
            self$VersionNumber <- VersionNumberObject
        }
        if (!is.null(MRecentTimesObject$m_Values)) {
            self$m_Values <- MRecentTimesObject$m_Values
        }
    }, toJSONString = function() {
        sprintf("{
           \"VersionNumber\": %s,
           \"m_Values\": [%s]
        }",
            self$VersionNumber$toJSON(), lapply(self$m_Values, function(x) paste(paste0("\"",
                x, "\""), sep = ",")))
    }, fromJSONString = function(MRecentTimesJson) {
        MRecentTimesObject <- jsonlite::fromJSON(MRecentTimesJson)
        VersionNumberObject <- VersionNumber$new()
        self$VersionNumber <- VersionNumberObject$fromJSON(jsonlite::toJSON(MRecentTimesObject$VersionNumber,
            auto_unbox = TRUE))
        self$m_Values <- MRecentTimesObject$m_Values
    }))
