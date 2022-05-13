# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MNano Class
#'
#' @field m_NanoConfig 
#' @field MagicNumber 
#' @field VersionNumber 
#' @field BackendVersion 
#' @field m_ErrorMsg 
#' @field m_NanoBackend 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MNano <- R6::R6Class(
  'MNano',
  public = list(
    `m_NanoConfig` = NULL,
    `MagicNumber` = NULL,
    `VersionNumber` = NULL,
    `BackendVersion` = NULL,
    `m_ErrorMsg` = NULL,
    `m_NanoBackend` = NULL,
    initialize = function(`m_NanoConfig`, `MagicNumber`, `VersionNumber`, `BackendVersion`, `m_ErrorMsg`, `m_NanoBackend`){
      if (!missing(`m_NanoConfig`)) {
        stopifnot(R6::is.R6(`m_NanoConfig`))
        self$`m_NanoConfig` <- `m_NanoConfig`
      }
      if (!missing(`MagicNumber`)) {
        stopifnot(R6::is.R6(`MagicNumber`))
        self$`MagicNumber` <- `MagicNumber`
      }
      if (!missing(`VersionNumber`)) {
        stopifnot(R6::is.R6(`VersionNumber`))
        self$`VersionNumber` <- `VersionNumber`
      }
      if (!missing(`BackendVersion`)) {
        stopifnot(is.numeric(`BackendVersion`), length(`BackendVersion`) == 1)
        self$`BackendVersion` <- `BackendVersion`
      }
      if (!missing(`m_ErrorMsg`)) {
        stopifnot(is.character(`m_ErrorMsg`), length(`m_ErrorMsg`) == 1)
        self$`m_ErrorMsg` <- `m_ErrorMsg`
      }
      if (!missing(`m_NanoBackend`)) {
        stopifnot(R6::is.R6(`m_NanoBackend`))
        self$`m_NanoBackend` <- `m_NanoBackend`
      }
    },
    toJSON = function() {
      MNanoObject <- list()
      if (!is.null(self$`m_NanoConfig`)) {
        MNanoObject[['m_NanoConfig']] <- self$`m_NanoConfig`$toJSON()
      }
      if (!is.null(self$`MagicNumber`)) {
        MNanoObject[['MagicNumber']] <- self$`MagicNumber`$toJSON()
      }
      if (!is.null(self$`VersionNumber`)) {
        MNanoObject[['VersionNumber']] <- self$`VersionNumber`$toJSON()
      }
      if (!is.null(self$`BackendVersion`)) {
        MNanoObject[['BackendVersion']] <- self$`BackendVersion`
      }
      if (!is.null(self$`m_ErrorMsg`)) {
        MNanoObject[['m_ErrorMsg']] <- self$`m_ErrorMsg`
      }
      if (!is.null(self$`m_NanoBackend`)) {
        MNanoObject[['m_NanoBackend']] <- self$`m_NanoBackend`$toJSON()
      }

      MNanoObject
    },
    fromJSON = function(MNanoJson) {
      MNanoObject <- jsonlite::fromJSON(MNanoJson, simplifyVector = FALSE)
      if (!is.null(MNanoObject$`m_NanoConfig`)) {
        m_NanoConfigObject <- MNanoConfig$new()
        m_NanoConfigObject$fromJSON(jsonlite::toJSON(MNanoObject$m_NanoConfig, auto_unbox = TRUE))
        self$`m_NanoConfig` <- m_NanoConfigObject
      }
      if (!is.null(MNanoObject$`MagicNumber`)) {
        MagicNumberObject <- MagicNumber$new()
        MagicNumberObject$fromJSON(jsonlite::toJSON(MNanoObject$MagicNumber, auto_unbox = TRUE))
        self$`MagicNumber` <- MagicNumberObject
      }
      if (!is.null(MNanoObject$`VersionNumber`)) {
        VersionNumberObject <- VersionNumber$new()
        VersionNumberObject$fromJSON(jsonlite::toJSON(MNanoObject$VersionNumber, auto_unbox = TRUE))
        self$`VersionNumber` <- VersionNumberObject
      }
      if (!is.null(MNanoObject$`BackendVersion`)) {
        self$`BackendVersion` <- MNanoObject$`BackendVersion`
      }
      if (!is.null(MNanoObject$`m_ErrorMsg`)) {
        self$`m_ErrorMsg` <- MNanoObject$`m_ErrorMsg`
      }
      if (!is.null(MNanoObject$`m_NanoBackend`)) {
        m_NanoBackendObject <- MNanoBackend$new()
        m_NanoBackendObject$fromJSON(jsonlite::toJSON(MNanoObject$m_NanoBackend, auto_unbox = TRUE))
        self$`m_NanoBackend` <- m_NanoBackendObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "m_NanoConfig": %s,
           "MagicNumber": %s,
           "VersionNumber": %s,
           "BackendVersion": %d,
           "m_ErrorMsg": %s,
           "m_NanoBackend": %s
        }',
        self$`m_NanoConfig`$toJSON(),
        self$`MagicNumber`$toJSON(),
        self$`VersionNumber`$toJSON(),
        self$`BackendVersion`,
        self$`m_ErrorMsg`,
        self$`m_NanoBackend`$toJSON()
      )
    },
    fromJSONString = function(MNanoJson) {
      MNanoObject <- jsonlite::fromJSON(MNanoJson, simplifyVector = FALSE)
      MNanoConfigObject <- MNanoConfig$new()
      self$`m_NanoConfig` <- MNanoConfigObject$fromJSON(jsonlite::toJSON(MNanoObject$m_NanoConfig, auto_unbox = TRUE))
      MagicNumberObject <- MagicNumber$new()
      self$`MagicNumber` <- MagicNumberObject$fromJSON(jsonlite::toJSON(MNanoObject$MagicNumber, auto_unbox = TRUE))
      VersionNumberObject <- VersionNumber$new()
      self$`VersionNumber` <- VersionNumberObject$fromJSON(jsonlite::toJSON(MNanoObject$VersionNumber, auto_unbox = TRUE))
      self$`BackendVersion` <- MNanoObject$`BackendVersion`
      self$`m_ErrorMsg` <- MNanoObject$`m_ErrorMsg`
      MNanoBackendObject <- MNanoBackend$new()
      self$`m_NanoBackend` <- MNanoBackendObject$fromJSON(jsonlite::toJSON(MNanoObject$m_NanoBackend, auto_unbox = TRUE))
    }
  )
)

