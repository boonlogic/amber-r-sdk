# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Uint32Array Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Uint32Array <- R6::R6Class(
  'Uint32Array',
  public = list(
    initialize = function(){
    },
    toJSON = function() {
      Uint32ArrayObject <- list()

      Uint32ArrayObject
    },
    fromJSON = function(Uint32ArrayJson) {
      Uint32ArrayObject <- jsonlite::fromJSON(Uint32ArrayJson, simplifyVector = FALSE)
    },
    toJSONString = function() {
       sprintf(
        '{
        }',
      )
    },
    fromJSONString = function(Uint32ArrayJson) {
      Uint32ArrayObject <- jsonlite::fromJSON(Uint32ArrayJson, simplifyVector = FALSE)
    }
  )
)

