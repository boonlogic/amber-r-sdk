# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostAuth2Request Class
#'
#' @field username 
#' @field password 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostAuth2Request <- R6::R6Class(
  'PostAuth2Request',
  public = list(
    `username` = NULL,
    `password` = NULL,
    initialize = function(`username`, `password`){
      if (!missing(`username`)) {
        stopifnot(is.character(`username`), length(`username`) == 1)
        self$`username` <- `username`
      }
      if (!missing(`password`)) {
        stopifnot(is.character(`password`), length(`password`) == 1)
        self$`password` <- `password`
      }
    },
    toJSON = function() {
      PostAuth2RequestObject <- list()
      if (!is.null(self$`username`)) {
        PostAuth2RequestObject[['username']] <- self$`username`
      }
      if (!is.null(self$`password`)) {
        PostAuth2RequestObject[['password']] <- self$`password`
      }

      PostAuth2RequestObject
    },
    fromJSON = function(PostAuth2RequestJson) {
      PostAuth2RequestObject <- jsonlite::fromJSON(PostAuth2RequestJson, simplifyVector = FALSE)
      if (!is.null(PostAuth2RequestObject$`username`)) {
        self$`username` <- PostAuth2RequestObject$`username`
      }
      if (!is.null(PostAuth2RequestObject$`password`)) {
        self$`password` <- PostAuth2RequestObject$`password`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "username": %s,
           "password": %s
        }',
        self$`username`,
        self$`password`
      )
    },
    fromJSONString = function(PostAuth2RequestJson) {
      PostAuth2RequestObject <- jsonlite::fromJSON(PostAuth2RequestJson, simplifyVector = FALSE)
      self$`username` <- PostAuth2RequestObject$`username`
      self$`password` <- PostAuth2RequestObject$`password`
    }
  )
)

