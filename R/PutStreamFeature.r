# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PutStreamFeature Class
#'
#' @field label 
#' @field value 
#' @field ts 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PutStreamFeature <- R6::R6Class(
  'PutStreamFeature',
  public = list(
    `label` = NULL,
    `value` = NULL,
    `ts` = NULL,
    initialize = function(`label`, `value`, `ts`){
      if (!missing(`label`)) {
        stopifnot(is.character(`label`), length(`label`) == 1)
        self$`label` <- `label`
      }
      if (!missing(`value`)) {
        stopifnot(is.numeric(`value`), length(`value`) == 1)
        self$`value` <- `value`
      }
      if (!missing(`ts`)) {
        stopifnot(is.character(`ts`), length(`ts`) == 1)
        self$`ts` <- `ts`
      }
    },
    toJSON = function() {
      PutStreamFeatureObject <- list()
      if (!is.null(self$`label`)) {
        PutStreamFeatureObject[['label']] <- self$`label`
      }
      if (!is.null(self$`value`)) {
        PutStreamFeatureObject[['value']] <- self$`value`
      }
      if (!is.null(self$`ts`)) {
        PutStreamFeatureObject[['ts']] <- self$`ts`
      }

      PutStreamFeatureObject
    },
    fromJSON = function(PutStreamFeatureJson) {
      PutStreamFeatureObject <- jsonlite::fromJSON(PutStreamFeatureJson)
      if (!is.null(PutStreamFeatureObject$`label`)) {
        self$`label` <- PutStreamFeatureObject$`label`
      }
      if (!is.null(PutStreamFeatureObject$`value`)) {
        self$`value` <- PutStreamFeatureObject$`value`
      }
      if (!is.null(PutStreamFeatureObject$`ts`)) {
        self$`ts` <- PutStreamFeatureObject$`ts`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "label": %s,
           "value": %d,
           "ts": %s
        }',
        self$`label`,
        self$`value`,
        self$`ts`
      )
    },
    fromJSONString = function(PutStreamFeatureJson) {
      PutStreamFeatureObject <- jsonlite::fromJSON(PutStreamFeatureJson)
      self$`label` <- PutStreamFeatureObject$`label`
      self$`value` <- PutStreamFeatureObject$`value`
      self$`ts` <- PutStreamFeatureObject$`ts`
    }
  )
)
