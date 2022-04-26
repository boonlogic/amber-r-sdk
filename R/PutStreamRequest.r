# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PutStreamRequest Class
#'
#' @field vector 
#' @field submitRule 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PutStreamRequest <- R6::R6Class(
  'PutStreamRequest',
  public = list(
    `vector` = NULL,
    `submitRule` = NULL,
    initialize = function(`vector`, `submitRule`){
      if (!missing(`vector`)) {
        stopifnot(is.list(`vector`), length(`vector`) != 0)
        lapply(`vector`, function(x) stopifnot(R6::is.R6(x)))
        self$`vector` <- `vector`
      }
      if (!missing(`submitRule`)) {
        stopifnot(is.character(`submitRule`), length(`submitRule`) == 1)
        self$`submitRule` <- `submitRule`
      }
    },
    toJSON = function() {
      PutStreamRequestObject <- list()
      if (!is.null(self$`vector`)) {
        PutStreamRequestObject[['vector']] <- lapply(self$`vector`, function(x) x$toJSON())
      }
      if (!is.null(self$`submitRule`)) {
        PutStreamRequestObject[['submitRule']] <- self$`submitRule`
      }

      PutStreamRequestObject
    },
    fromJSON = function(PutStreamRequestJson) {
      PutStreamRequestObject <- jsonlite::fromJSON(PutStreamRequestJson)
      if (!is.null(PutStreamRequestObject$`vector`)) {
        self$`vector` <- lapply(PutStreamRequestObject$`vector`, function(x) {
          vectorObject <- PutStreamFeature$new()
          vectorObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          vectorObject
        })
      }
      if (!is.null(PutStreamRequestObject$`submitRule`)) {
        self$`submitRule` <- PutStreamRequestObject$`submitRule`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "vector": [%s],
           "submitRule": %s
        }',
        lapply(self$`vector`, function(x) paste(x$toJSON(), sep=",")),
        self$`submitRule`
      )
    },
    fromJSONString = function(PutStreamRequestJson) {
      PutStreamRequestObject <- jsonlite::fromJSON(PutStreamRequestJson)
      self$`vector` <- lapply(PutStreamRequestObject$`vector`, function(x) PutStreamFeature$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$`submitRule` <- PutStreamRequestObject$`submitRule`
    }
  )
)
