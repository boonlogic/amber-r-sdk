# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' EndpointUsageInfo Class
#'
#' @field callsTotal 
#' @field callsThisPeriod 
#' @field lastCalled 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
EndpointUsageInfo <- R6::R6Class("EndpointUsageInfo", public = list(callsTotal = NULL,
    callsThisPeriod = NULL, lastCalled = NULL, initialize = function(callsTotal,
        callsThisPeriod, lastCalled) {
        if (!missing(callsTotal)) {
            stopifnot(is.numeric(callsTotal), length(callsTotal) == 1)
            stopifnot(R6::is.R6(callsTotal))
            self$callsTotal <- callsTotal
        }
        if (!missing(callsThisPeriod)) {
            stopifnot(is.numeric(callsThisPeriod), length(callsThisPeriod) == 1)
            stopifnot(R6::is.R6(callsThisPeriod))
            self$callsThisPeriod <- callsThisPeriod
        }
        if (!missing(lastCalled)) {
            stopifnot(is.character(lastCalled), length(lastCalled) == 1)
            self$lastCalled <- lastCalled
        }
    }, toJSON = function() {
        EndpointUsageInfoObject <- list()
        if (!is.null(self$callsTotal)) {
            EndpointUsageInfoObject[["callsTotal"]] <- self$callsTotal$toJSON()
        }
        if (!is.null(self$callsThisPeriod)) {
            EndpointUsageInfoObject[["callsThisPeriod"]] <- self$callsThisPeriod$toJSON()
        }
        if (!is.null(self$lastCalled)) {
            EndpointUsageInfoObject[["lastCalled"]] <- self$lastCalled
        }

        EndpointUsageInfoObject
    }, fromJSON = function(EndpointUsageInfoJson) {
        EndpointUsageInfoObject <- jsonlite::fromJSON(EndpointUsageInfoJson)
        if (!is.null(EndpointUsageInfoObject$callsTotal)) {
            callsTotalObject <- BigDecimal$new()
            callsTotalObject$fromJSON(jsonlite::toJSON(EndpointUsageInfoObject$callsTotal,
                auto_unbox = TRUE))
            self$callsTotal <- callsTotalObject
        }
        if (!is.null(EndpointUsageInfoObject$callsThisPeriod)) {
            callsThisPeriodObject <- BigDecimal$new()
            callsThisPeriodObject$fromJSON(jsonlite::toJSON(EndpointUsageInfoObject$callsThisPeriod,
                auto_unbox = TRUE))
            self$callsThisPeriod <- callsThisPeriodObject
        }
        if (!is.null(EndpointUsageInfoObject$lastCalled)) {
            self$lastCalled <- EndpointUsageInfoObject$lastCalled
        }
    }, toJSONString = function() {
        sprintf("{
           \"callsTotal\": %s,
           \"callsThisPeriod\": %s,
           \"lastCalled\": %s
        }",
            self$callsTotal$toJSON(), self$callsThisPeriod$toJSON(), self$lastCalled)
    }, fromJSONString = function(EndpointUsageInfoJson) {
        EndpointUsageInfoObject <- jsonlite::fromJSON(EndpointUsageInfoJson)
        BigDecimalObject <- BigDecimal$new()
        self$callsTotal <- BigDecimalObject$fromJSON(jsonlite::toJSON(EndpointUsageInfoObject$callsTotal,
            auto_unbox = TRUE))
        BigDecimalObject <- BigDecimal$new()
        self$callsThisPeriod <- BigDecimalObject$fromJSON(jsonlite::toJSON(EndpointUsageInfoObject$callsThisPeriod,
            auto_unbox = TRUE))
        self$lastCalled <- EndpointUsageInfoObject$lastCalled
    }))