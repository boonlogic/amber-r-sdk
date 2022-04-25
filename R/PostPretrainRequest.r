# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostPretrainRequest Class
#'
#' @field data 
#' @field autotuneConfig 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostPretrainRequest <- R6::R6Class(
  'PostPretrainRequest',
  public = list(
    `data` = NULL,
    `autotuneConfig` = NULL,
    initialize = function(`data`, `autotuneConfig`){
      if (!missing(`data`)) {
        stopifnot(is.character(`data`), length(`data`) == 1)
        self$`data` <- `data`
      }
      if (!missing(`autotuneConfig`)) {
        self$`autotuneConfig` <- `autotuneConfig`
      }
    },
    toJSON = function() {
      PostPretrainRequestObject <- list()
      if (!is.null(self$`data`)) {
        PostPretrainRequestObject[['data']] <- self$`data`
      }
      if (!is.null(self$`autotuneConfig`)) {
        PostPretrainRequestObject[['autotuneConfig']] <- self$`autotuneConfig`
      }

      PostPretrainRequestObject
    },
    fromJSON = function(PostPretrainRequestJson) {
      PostPretrainRequestObject <- jsonlite::fromJSON(PostPretrainRequestJson)
      if (!is.null(PostPretrainRequestObject$`data`)) {
        self$`data` <- PostPretrainRequestObject$`data`
      }
      if (!is.null(PostPretrainRequestObject$`autotuneConfig`)) {
        self$`autotuneConfig` <- PostPretrainRequestObject$`autotuneConfig`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "data": %s,
           "autotuneConfig": %s
        }',
        self$`data`,
        self$`autotuneConfig`
      )
    },
    fromJSONString = function(PostPretrainRequestJson) {
      PostPretrainRequestObject <- jsonlite::fromJSON(PostPretrainRequestJson)
      self$`data` <- PostPretrainRequestObject$`data`
      self$`autotuneConfig` <- PostPretrainRequestObject$`autotuneConfig`
    }
  )
)
