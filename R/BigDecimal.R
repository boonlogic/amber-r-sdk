# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
#' BigDecimal Class
#'
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BigDecimal <- R6::R6Class(
  'BigDecimal',
  public = list(
    Uint64 = NULL,
    initialize = function(){
      self$Uint64
    },
    toJSON = function() {
      self$Uint64 <- 0

      self$Uint64
    },
    fromJSON = function(Uint64Json) {
      self$Uint64 <- jsonlite::fromJSON(Uint64Json)
    },
    toJSONString = function() {
       sprintf(
        '{
        }',
      )
    },
    fromJSONString = function(Uint64Json) {
      self$Uint64 <- jsonlite::fromJSON(Uint64Json)
    }
  )
)