# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostPretrainResponse Class
#'
#' @field state 
#' @field message 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostPretrainResponse <- R6::R6Class("PostPretrainResponse", public = list(state = NULL,
    message = NULL, initialize = function(state, message) {
        if (!missing(state)) {
            stopifnot(is.character(state), length(state) == 1)
            self$state <- state
        }
        if (!missing(message)) {
            stopifnot(is.character(message), length(message) == 1)
            self$message <- message
        }
    }, toJSON = function() {
        PostPretrainResponseObject <- list()
        if (!is.null(self$state)) {
            PostPretrainResponseObject[["state"]] <- self$state
        }
        if (!is.null(self$message)) {
            PostPretrainResponseObject[["message"]] <- self$message
        }

        PostPretrainResponseObject
    }, fromJSON = function(PostPretrainResponseJson) {
        PostPretrainResponseObject <- jsonlite::fromJSON(PostPretrainResponseJson)
        if (!is.null(PostPretrainResponseObject$state)) {
            self$state <- PostPretrainResponseObject$state
        }
        if (!is.null(PostPretrainResponseObject$message)) {
            self$message <- PostPretrainResponseObject$message
        }
    }, toJSONString = function() {
        sprintf("{
           \"state\": %s,
           \"message\": %s
        }",
            self$state, self$message)
    }, fromJSONString = function(PostPretrainResponseJson) {
        PostPretrainResponseObject <- jsonlite::fromJSON(PostPretrainResponseJson)
        self$state <- PostPretrainResponseObject$state
        self$message <- PostPretrainResponseObject$message
    }))