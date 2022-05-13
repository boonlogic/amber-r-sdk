# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GetPretrainResponse Class
#'
#' @field state 
#' @field message 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GetPretrainResponse <- R6::R6Class(
  'GetPretrainResponse',
  public = list(
    `state` = NULL,
    `message` = NULL,
    initialize = function(`state`, `message`){
      if (!missing(`state`)) {
        stopifnot(is.character(`state`), length(`state`) == 1)
        self$`state` <- `state`
      }
      if (!missing(`message`)) {
        stopifnot(is.character(`message`), length(`message`) == 1)
        self$`message` <- `message`
      }
    },
    toJSON = function() {
      GetPretrainResponseObject <- list()
      if (!is.null(self$`state`)) {
        GetPretrainResponseObject[['state']] <- self$`state`
      }
      if (!is.null(self$`message`)) {
        GetPretrainResponseObject[['message']] <- self$`message`
      }

      GetPretrainResponseObject
    },
    fromJSON = function(GetPretrainResponseJson) {
      GetPretrainResponseObject <- jsonlite::fromJSON(GetPretrainResponseJson, simplifyVector = FALSE)
      if (!is.null(GetPretrainResponseObject$`state`)) {
        self$`state` <- GetPretrainResponseObject$`state`
      }
      if (!is.null(GetPretrainResponseObject$`message`)) {
        self$`message` <- GetPretrainResponseObject$`message`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "state": %s,
           "message": %s
        }',
        self$`state`,
        self$`message`
      )
    },
    fromJSONString = function(GetPretrainResponseJson) {
      GetPretrainResponseObject <- jsonlite::fromJSON(GetPretrainResponseJson, simplifyVector = FALSE)
      self$`state` <- GetPretrainResponseObject$`state`
      self$`message` <- GetPretrainResponseObject$`message`
    }
  )
)

