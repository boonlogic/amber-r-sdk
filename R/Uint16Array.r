# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Uint16Array Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Uint16Array <- R6::R6Class("Uint16Array", public = list(initialize = function() {
}, toJSON = function() {
    Uint16ArrayObject <- list()

    Uint16ArrayObject
}, fromJSON = function(Uint16ArrayJson) {
    Uint16ArrayObject <- jsonlite::fromJSON(Uint16ArrayJson)
}, toJSONString = function() {
    sprintf("{
        }", )
}, fromJSONString = function(Uint16ArrayJson) {
    Uint16ArrayObject <- jsonlite::fromJSON(Uint16ArrayJson)
}))