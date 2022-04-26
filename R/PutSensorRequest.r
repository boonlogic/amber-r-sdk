# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PutSensorRequest Class
#'
#' @field label 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PutSensorRequest <- R6::R6Class("PutSensorRequest", public = list(label = NULL, initialize = function(label) {
    if (!missing(label)) {
        stopifnot(is.character(label), length(label) == 1)
        self$label <- label
    }
}, toJSON = function() {
    PutSensorRequestObject <- list()
    if (!is.null(self$label)) {
        PutSensorRequestObject[["label"]] <- self$label
    }

    PutSensorRequestObject
}, fromJSON = function(PutSensorRequestJson) {
    PutSensorRequestObject <- jsonlite::fromJSON(PutSensorRequestJson)
    if (!is.null(PutSensorRequestObject$label)) {
        self$label <- PutSensorRequestObject$label
    }
}, toJSONString = function() {
    sprintf("{
           \"label\": %s
        }", self$label)
}, fromJSONString = function(PutSensorRequestJson) {
    PutSensorRequestObject <- jsonlite::fromJSON(PutSensorRequestJson)
    self$label <- PutSensorRequestObject$label
}))
