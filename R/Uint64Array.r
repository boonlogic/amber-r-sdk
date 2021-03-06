# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Uint64Array Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Uint64Array <- R6::R6Class(
  'Uint64Array',
  public = list(
    initialize = function(){
    },
    toJSON = function() {
      Uint64ArrayObject <- list()

      Uint64ArrayObject
    },
    fromJSON = function(Uint64ArrayJson) {
      Uint64ArrayObject <- jsonlite::fromJSON(Uint64ArrayJson, simplifyVector = FALSE)
    },
    toJSONString = function() {
       sprintf(
        '{
        }',
      )
    },
    fromJSONString = function(Uint64ArrayJson) {
      Uint64ArrayObject <- jsonlite::fromJSON(Uint64ArrayJson, simplifyVector = FALSE)
    }
  )
)

