# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' VersionNumber Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
VersionNumber <- R6::R6Class("VersionNumber", public = list(initialize = function() {
}, toJSON = function() {
    VersionNumberObject <- list()

    VersionNumberObject
}, fromJSON = function(VersionNumberJson) {
    VersionNumberObject <- jsonlite::fromJSON(VersionNumberJson)
}, toJSONString = function() {
    sprintf("{
        }", )
}, fromJSONString = function(VersionNumberJson) {
    VersionNumberObject <- jsonlite::fromJSON(VersionNumberJson)
}))
