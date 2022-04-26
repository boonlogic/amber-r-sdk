# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' PostAuth2Response Class
#'
#' @field idToken 
#' @field refreshToken 
#' @field expiresIn 
#' @field tokenType 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PostAuth2Response <- R6::R6Class("PostAuth2Response", public = list(idToken = NULL,
    refreshToken = NULL, expiresIn = NULL, tokenType = NULL, initialize = function(idToken,
        refreshToken, expiresIn, tokenType) {
        if (!missing(idToken)) {
            stopifnot(is.character(idToken), length(idToken) == 1)
            self$idToken <- idToken
        }
        if (!missing(refreshToken)) {
            stopifnot(is.character(refreshToken), length(refreshToken) == 1)
            self$refreshToken <- refreshToken
        }
        if (!missing(expiresIn)) {
            stopifnot(is.character(expiresIn), length(expiresIn) == 1)
            self$expiresIn <- expiresIn
        }
        if (!missing(tokenType)) {
            stopifnot(is.character(tokenType), length(tokenType) == 1)
            self$tokenType <- tokenType
        }
    }, toJSON = function() {
        PostAuth2ResponseObject <- list()
        if (!is.null(self$idToken)) {
            PostAuth2ResponseObject[["idToken"]] <- self$idToken
        }
        if (!is.null(self$refreshToken)) {
            PostAuth2ResponseObject[["refreshToken"]] <- self$refreshToken
        }
        if (!is.null(self$expiresIn)) {
            PostAuth2ResponseObject[["expiresIn"]] <- self$expiresIn
        }
        if (!is.null(self$tokenType)) {
            PostAuth2ResponseObject[["tokenType"]] <- self$tokenType
        }

        PostAuth2ResponseObject
    }, fromJSON = function(PostAuth2ResponseJson) {
        PostAuth2ResponseObject <- jsonlite::fromJSON(PostAuth2ResponseJson)
        if (!is.null(PostAuth2ResponseObject$idToken)) {
            self$idToken <- PostAuth2ResponseObject$idToken
        }
        if (!is.null(PostAuth2ResponseObject$refreshToken)) {
            self$refreshToken <- PostAuth2ResponseObject$refreshToken
        }
        if (!is.null(PostAuth2ResponseObject$expiresIn)) {
            self$expiresIn <- PostAuth2ResponseObject$expiresIn
        }
        if (!is.null(PostAuth2ResponseObject$tokenType)) {
            self$tokenType <- PostAuth2ResponseObject$tokenType
        }
    }, toJSONString = function() {
        sprintf("{
           \"idToken\": %s,
           \"refreshToken\": %s,
           \"expiresIn\": %s,
           \"tokenType\": %s
        }",
            self$idToken, self$refreshToken, self$expiresIn, self$tokenType)
    }, fromJSONString = function(PostAuth2ResponseJson) {
        PostAuth2ResponseObject <- jsonlite::fromJSON(PostAuth2ResponseJson)
        self$idToken <- PostAuth2ResponseObject$idToken
        self$refreshToken <- PostAuth2ResponseObject$refreshToken
        self$expiresIn <- PostAuth2ResponseObject$expiresIn
        self$tokenType <- PostAuth2ResponseObject$tokenType
    }))
