# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Int32Array Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Int32Array <- R6::R6Class(
  'Int32Array',
  public = list(
    initialize = function(){
    },
    toJSON = function() {
      Int32ArrayObject <- list()

      Int32ArrayObject
    },
    fromJSON = function(Int32ArrayJson) {
      Int32ArrayObject <- jsonlite::fromJSON(Int32ArrayJson, simplifyVector = FALSE)
    },
    toJSONString = function() {
       sprintf(
        '{
        }',
      )
    },
    fromJSONString = function(Int32ArrayJson) {
      Int32ArrayObject <- jsonlite::fromJSON(Int32ArrayJson, simplifyVector = FALSE)
    }
  )
)

