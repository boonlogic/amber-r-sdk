# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostStreamRequest Class
#'
#' @field saveImage 
#' @field data 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostStreamRequest <- R6::R6Class("PostStreamRequest", public = list(saveImage = NULL,
    data = NULL, initialize = function(saveImage, data) {
        if (!missing(saveImage)) {
            self$saveImage <- saveImage
        }
        if (!missing(data)) {
            stopifnot(is.character(data), length(data) == 1)
            self$data <- data
        }
    }, toJSON = function() {
        PostStreamRequestObject <- list()
        if (!is.null(self$saveImage)) {
            PostStreamRequestObject[["saveImage"]] <- self$saveImage
        }
        if (!is.null(self$data)) {
            PostStreamRequestObject[["data"]] <- self$data
        }

        PostStreamRequestObject
    }, fromJSON = function(PostStreamRequestJson) {
        PostStreamRequestObject <- jsonlite::fromJSON(PostStreamRequestJson)
        if (!is.null(PostStreamRequestObject$saveImage)) {
            self$saveImage <- PostStreamRequestObject$saveImage
        }
        if (!is.null(PostStreamRequestObject$data)) {
            self$data <- PostStreamRequestObject$data
        }
    }, toJSONString = function() {
        sprintf("{
           \"saveImage\": %s,
           \"data\": %s
        }",
            self$saveImage, self$data)
    }, fromJSONString = function(PostStreamRequestJson) {
        PostStreamRequestObject <- jsonlite::fromJSON(PostStreamRequestJson)
        self$saveImage <- PostStreamRequestObject$saveImage
        self$data <- PostStreamRequestObject$data
    }))
