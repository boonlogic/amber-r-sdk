# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PutConfigRequest Class
#'
#' @field features 
#' @field streaming 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PutConfigRequest <- R6::R6Class("PutConfigRequest", public = list(features = NULL,
    streaming = NULL, initialize = function(features, streaming) {
        if (!missing(features)) {
            stopifnot(is.list(features), length(features) != 0)
            lapply(features, function(x) stopifnot(R6::is.R6(x)))
            self$features <- features
        }
        if (!missing(streaming)) {
            stopifnot(R6::is.R6(streaming))
            self$streaming <- streaming
        }
    }, toJSON = function() {
        PutConfigRequestObject <- list()
        if (!is.null(self$features)) {
            PutConfigRequestObject[["features"]] <- lapply(self$features, function(x) x$toJSON())
        }
        if (!is.null(self$streaming)) {
            PutConfigRequestObject[["streaming"]] <- self$streaming$toJSON()
        }

        PutConfigRequestObject
    }, fromJSON = function(PutConfigRequestJson) {
        PutConfigRequestObject <- jsonlite::fromJSON(PutConfigRequestJson)
        if (!is.null(PutConfigRequestObject$features)) {
            self$features <- lapply(PutConfigRequestObject$features, function(x) {
                featuresObject <- FusionConfig$new()
                featuresObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
                featuresObject
            })
        }
        if (!is.null(PutConfigRequestObject$streaming)) {
            streamingObject <- StreamingParameters$new()
            streamingObject$fromJSON(jsonlite::toJSON(PutConfigRequestObject$streaming,
                auto_unbox = TRUE))
            self$streaming <- streamingObject
        }
    }, toJSONString = function() {
        sprintf("{
           \"features\": [%s],
           \"streaming\": %s
        }",
            lapply(self$features, function(x) paste(x$toJSON(), sep = ",")), self$streaming$toJSON())
    }, fromJSONString = function(PutConfigRequestJson) {
        PutConfigRequestObject <- jsonlite::fromJSON(PutConfigRequestJson)
        self$features <- lapply(PutConfigRequestObject$features, function(x) FusionConfig$new()$fromJSON(jsonlite::toJSON(x,
            auto_unbox = TRUE)))
        StreamingParametersObject <- StreamingParameters$new()
        self$streaming <- StreamingParametersObject$fromJSON(jsonlite::toJSON(PutConfigRequestObject$streaming,
            auto_unbox = TRUE))
    }))