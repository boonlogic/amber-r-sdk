# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MRecentAMs Class
#'
#' @field VersionNumber 
#' @field m_Values 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MRecentAMs <- R6::R6Class(
  'MRecentAMs',
  public = list(
    `VersionNumber` = NULL,
    `m_Values` = NULL,
    initialize = function(`VersionNumber`, `m_Values`){
      if (!missing(`VersionNumber`)) {
        stopifnot(R6::is.R6(`VersionNumber`))
        self$`VersionNumber` <- `VersionNumber`
      }
      if (!missing(`m_Values`)) {
        stopifnot(is.list(`m_Values`), length(`m_Values`) != 0)
        lapply(`m_Values`, function(x) stopifnot(is.character(x)))
        self$`m_Values` <- `m_Values`
      }
    },
    toJSON = function() {
      MRecentAMsObject <- list()
      if (!is.null(self$`VersionNumber`)) {
        MRecentAMsObject[['VersionNumber']] <- self$`VersionNumber`$toJSON()
      }
      if (!is.null(self$`m_Values`)) {
        MRecentAMsObject[['m_Values']] <- self$`m_Values`
      }

      MRecentAMsObject
    },
    fromJSON = function(MRecentAMsJson) {
      MRecentAMsObject <- jsonlite::fromJSON(MRecentAMsJson, simplifyVector = FALSE)
      if (!is.null(MRecentAMsObject$`VersionNumber`)) {
        VersionNumberObject <- VersionNumber$new()
        VersionNumberObject$fromJSON(jsonlite::toJSON(MRecentAMsObject$VersionNumber, auto_unbox = TRUE))
        self$`VersionNumber` <- VersionNumberObject
      }
      if (!is.null(MRecentAMsObject$`m_Values`)) {
        self$`m_Values` <- MRecentAMsObject$`m_Values`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "VersionNumber": %s,
           "m_Values": [%s]
        }',
        self$`VersionNumber`$toJSON(),
        lapply(self$`m_Values`, function(x) paste(paste0('"', x, '"'), sep=","))
      )
    },
    fromJSONString = function(MRecentAMsJson) {
      MRecentAMsObject <- jsonlite::fromJSON(MRecentAMsJson, simplifyVector = FALSE)
      VersionNumberObject <- VersionNumber$new()
      self$`VersionNumber` <- VersionNumberObject$fromJSON(jsonlite::toJSON(MRecentAMsObject$VersionNumber, auto_unbox = TRUE))
      self$`m_Values` <- MRecentAMsObject$`m_Values`
    }
  )
)

