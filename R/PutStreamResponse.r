# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PutStreamResponse Class
#'
#' @field vector 
#' @field vectorCSV 
#' @field results 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PutStreamResponse <- R6::R6Class("PutStreamResponse", public = list(vector = NULL,
    vectorCSV = NULL, results = NULL, initialize = function(vector, vectorCSV, results) {
        if (!missing(vector)) {
            stopifnot(R6::is.R6(vector))
            self$vector <- vector
        }
        if (!missing(vectorCSV)) {
            stopifnot(is.character(vectorCSV), length(vectorCSV) == 1)
            self$vectorCSV <- vectorCSV
        }
        if (!missing(results)) {
            stopifnot(R6::is.R6(results))
            self$results <- results
        }
    }, toJSON = function() {
        PutStreamResponseObject <- list()
        if (!is.null(self$vector)) {
            PutStreamResponseObject[["vector"]] <- self$vector$toJSON()
        }
        if (!is.null(self$vectorCSV)) {
            PutStreamResponseObject[["vectorCSV"]] <- self$vectorCSV
        }
        if (!is.null(self$results)) {
            PutStreamResponseObject[["results"]] <- self$results$toJSON()
        }

        PutStreamResponseObject
    }, fromJSON = function(PutStreamResponseJson) {
        PutStreamResponseObject <- jsonlite::fromJSON(PutStreamResponseJson)
        if (!is.null(PutStreamResponseObject$vector)) {
            vectorObject <- MayContainNullsArray$new()
            vectorObject$fromJSON(jsonlite::toJSON(PutStreamResponseObject$vector,
                auto_unbox = TRUE))
            self$vector <- vectorObject
        }
        if (!is.null(PutStreamResponseObject$vectorCSV)) {
            self$vectorCSV <- PutStreamResponseObject$vectorCSV
        }
        if (!is.null(PutStreamResponseObject$results)) {
            resultsObject <- PostStreamResponse$new()
            resultsObject$fromJSON(jsonlite::toJSON(PutStreamResponseObject$results,
                auto_unbox = TRUE))
            self$results <- resultsObject
        }
    }, toJSONString = function() {
        sprintf("{
           \"vector\": %s,
           \"vectorCSV\": %s,
           \"results\": %s
        }",
            self$vector$toJSON(), self$vectorCSV, self$results$toJSON())
    }, fromJSONString = function(PutStreamResponseJson) {
        PutStreamResponseObject <- jsonlite::fromJSON(PutStreamResponseJson)
        MayContainNullsArrayObject <- MayContainNullsArray$new()
        self$vector <- MayContainNullsArrayObject$fromJSON(jsonlite::toJSON(PutStreamResponseObject$vector,
            auto_unbox = TRUE))
        self$vectorCSV <- PutStreamResponseObject$vectorCSV
        PostStreamResponseObject <- PostStreamResponse$new()
        self$results <- PostStreamResponseObject$fromJSON(jsonlite::toJSON(PutStreamResponseObject$results,
            auto_unbox = TRUE))
    }))
