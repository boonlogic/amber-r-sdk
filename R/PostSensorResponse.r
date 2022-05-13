# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostSensorResponse Class
#'
#' @field label 
#' @field sensorId 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostSensorResponse <- R6::R6Class(
  'PostSensorResponse',
  public = list(
    `label` = NULL,
    `sensorId` = NULL,
    initialize = function(`label`, `sensorId`){
      if (!missing(`label`)) {
        stopifnot(is.character(`label`), length(`label`) == 1)
        self$`label` <- `label`
      }
      if (!missing(`sensorId`)) {
        stopifnot(is.character(`sensorId`), length(`sensorId`) == 1)
        self$`sensorId` <- `sensorId`
      }
    },
    toJSON = function() {
      PostSensorResponseObject <- list()
      if (!is.null(self$`label`)) {
        PostSensorResponseObject[['label']] <- self$`label`
      }
      if (!is.null(self$`sensorId`)) {
        PostSensorResponseObject[['sensorId']] <- self$`sensorId`
      }

      PostSensorResponseObject
    },
    fromJSON = function(PostSensorResponseJson) {
      PostSensorResponseObject <- jsonlite::fromJSON(PostSensorResponseJson, simplifyVector = FALSE)
      if (!is.null(PostSensorResponseObject$`label`)) {
        self$`label` <- PostSensorResponseObject$`label`
      }
      if (!is.null(PostSensorResponseObject$`sensorId`)) {
        self$`sensorId` <- PostSensorResponseObject$`sensorId`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "label": %s,
           "sensorId": %s
        }',
        self$`label`,
        self$`sensorId`
      )
    },
    fromJSONString = function(PostSensorResponseJson) {
      PostSensorResponseObject <- jsonlite::fromJSON(PostSensorResponseJson, simplifyVector = FALSE)
      self$`label` <- PostSensorResponseObject$`label`
      self$`sensorId` <- PostSensorResponseObject$`sensorId`
    }
  )
)

