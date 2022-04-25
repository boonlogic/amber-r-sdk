# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' MAutotune Class
#'
#' @field VersionNumber 
#' @field m_AutotuningInProgress 
#' @field m_PercentComplete 
#' @field m_AutotuningSucceeded 
#' @field m_NumPatternsToAutotune 
#' @field m_ErrorStringBuffer 
#' @field m_FeaturesToTuneArray 
#' @field m_NCP 
#' @field m_AP 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MAutotune <- R6::R6Class(
  'MAutotune',
  public = list(
    `VersionNumber` = NULL,
    `m_AutotuningInProgress` = NULL,
    `m_PercentComplete` = NULL,
    `m_AutotuningSucceeded` = NULL,
    `m_NumPatternsToAutotune` = NULL,
    `m_ErrorStringBuffer` = NULL,
    `m_FeaturesToTuneArray` = NULL,
    `m_NCP` = NULL,
    `m_AP` = NULL,
    initialize = function(`VersionNumber`, `m_AutotuningInProgress`, `m_PercentComplete`, `m_AutotuningSucceeded`, `m_NumPatternsToAutotune`, `m_ErrorStringBuffer`, `m_FeaturesToTuneArray`, `m_NCP`, `m_AP`){
      if (!missing(`VersionNumber`)) {
        stopifnot(R6::is.R6(`VersionNumber`))
        self$`VersionNumber` <- `VersionNumber`
      }
      if (!missing(`m_AutotuningInProgress`)) {
        self$`m_AutotuningInProgress` <- `m_AutotuningInProgress`
      }
      if (!missing(`m_PercentComplete`)) {
        stopifnot(is.numeric(`m_PercentComplete`), length(`m_PercentComplete`) == 1)
        stopifnot(R6::is.R6(`m_PercentComplete`))
        self$`m_PercentComplete` <- `m_PercentComplete`
      }
      if (!missing(`m_AutotuningSucceeded`)) {
        self$`m_AutotuningSucceeded` <- `m_AutotuningSucceeded`
      }
      if (!missing(`m_NumPatternsToAutotune`)) {
        stopifnot(is.numeric(`m_NumPatternsToAutotune`), length(`m_NumPatternsToAutotune`) == 1)
        self$`m_NumPatternsToAutotune` <- `m_NumPatternsToAutotune`
      }
      if (!missing(`m_ErrorStringBuffer`)) {
        stopifnot(is.character(`m_ErrorStringBuffer`), length(`m_ErrorStringBuffer`) == 1)
        self$`m_ErrorStringBuffer` <- `m_ErrorStringBuffer`
      }
      if (!missing(`m_FeaturesToTuneArray`)) {
        stopifnot(is.list(`m_FeaturesToTuneArray`), length(`m_FeaturesToTuneArray`) != 0)
        lapply(`m_FeaturesToTuneArray`, function(x) stopifnot(is.character(x)))
        self$`m_FeaturesToTuneArray` <- `m_FeaturesToTuneArray`
      }
      if (!missing(`m_NCP`)) {
        stopifnot(R6::is.R6(`m_NCP`))
        self$`m_NCP` <- `m_NCP`
      }
      if (!missing(`m_AP`)) {
        stopifnot(R6::is.R6(`m_AP`))
        self$`m_AP` <- `m_AP`
      }
    },
    toJSON = function() {
      MAutotuneObject <- list()
      if (!is.null(self$`VersionNumber`)) {
        MAutotuneObject[['VersionNumber']] <- self$`VersionNumber`$toJSON()
      }
      if (!is.null(self$`m_AutotuningInProgress`)) {
        MAutotuneObject[['m_AutotuningInProgress']] <- self$`m_AutotuningInProgress`
      }
      if (!is.null(self$`m_PercentComplete`)) {
        MAutotuneObject[['m_PercentComplete']] <- self$`m_PercentComplete`$toJSON()
      }
      if (!is.null(self$`m_AutotuningSucceeded`)) {
        MAutotuneObject[['m_AutotuningSucceeded']] <- self$`m_AutotuningSucceeded`
      }
      if (!is.null(self$`m_NumPatternsToAutotune`)) {
        MAutotuneObject[['m_NumPatternsToAutotune']] <- self$`m_NumPatternsToAutotune`
      }
      if (!is.null(self$`m_ErrorStringBuffer`)) {
        MAutotuneObject[['m_ErrorStringBuffer']] <- self$`m_ErrorStringBuffer`
      }
      if (!is.null(self$`m_FeaturesToTuneArray`)) {
        MAutotuneObject[['m_FeaturesToTuneArray']] <- self$`m_FeaturesToTuneArray`
      }
      if (!is.null(self$`m_NCP`)) {
        MAutotuneObject[['m_NCP']] <- self$`m_NCP`$toJSON()
      }
      if (!is.null(self$`m_AP`)) {
        MAutotuneObject[['m_AP']] <- self$`m_AP`$toJSON()
      }

      MAutotuneObject
    },
    fromJSON = function(MAutotuneJson) {
      MAutotuneObject <- jsonlite::fromJSON(MAutotuneJson)
      if (!is.null(MAutotuneObject$`VersionNumber`)) {
        VersionNumberObject <- VersionNumber$new()
        VersionNumberObject$fromJSON(jsonlite::toJSON(MAutotuneObject$VersionNumber, auto_unbox = TRUE))
        self$`VersionNumber` <- VersionNumberObject
      }
      if (!is.null(MAutotuneObject$`m_AutotuningInProgress`)) {
        self$`m_AutotuningInProgress` <- MAutotuneObject$`m_AutotuningInProgress`
      }
      if (!is.null(MAutotuneObject$`m_PercentComplete`)) {
        m_PercentCompleteObject <- BigDecimal$new()
        m_PercentCompleteObject$fromJSON(jsonlite::toJSON(MAutotuneObject$m_PercentComplete, auto_unbox = TRUE))
        self$`m_PercentComplete` <- m_PercentCompleteObject
      }
      if (!is.null(MAutotuneObject$`m_AutotuningSucceeded`)) {
        self$`m_AutotuningSucceeded` <- MAutotuneObject$`m_AutotuningSucceeded`
      }
      if (!is.null(MAutotuneObject$`m_NumPatternsToAutotune`)) {
        self$`m_NumPatternsToAutotune` <- MAutotuneObject$`m_NumPatternsToAutotune`
      }
      if (!is.null(MAutotuneObject$`m_ErrorStringBuffer`)) {
        self$`m_ErrorStringBuffer` <- MAutotuneObject$`m_ErrorStringBuffer`
      }
      if (!is.null(MAutotuneObject$`m_FeaturesToTuneArray`)) {
        self$`m_FeaturesToTuneArray` <- MAutotuneObject$`m_FeaturesToTuneArray`
      }
      if (!is.null(MAutotuneObject$`m_NCP`)) {
        m_NCPObject <- MNCP$new()
        m_NCPObject$fromJSON(jsonlite::toJSON(MAutotuneObject$m_NCP, auto_unbox = TRUE))
        self$`m_NCP` <- m_NCPObject
      }
      if (!is.null(MAutotuneObject$`m_AP`)) {
        m_APObject <- MAP$new()
        m_APObject$fromJSON(jsonlite::toJSON(MAutotuneObject$m_AP, auto_unbox = TRUE))
        self$`m_AP` <- m_APObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "VersionNumber": %s,
           "m_AutotuningInProgress": %s,
           "m_PercentComplete": %s,
           "m_AutotuningSucceeded": %s,
           "m_NumPatternsToAutotune": %d,
           "m_ErrorStringBuffer": %s,
           "m_FeaturesToTuneArray": [%s],
           "m_NCP": %s,
           "m_AP": %s
        }',
        self$`VersionNumber`$toJSON(),
        self$`m_AutotuningInProgress`,
        self$`m_PercentComplete`$toJSON(),
        self$`m_AutotuningSucceeded`,
        self$`m_NumPatternsToAutotune`,
        self$`m_ErrorStringBuffer`,
        lapply(self$`m_FeaturesToTuneArray`, function(x) paste(paste0('"', x, '"'), sep=",")),
        self$`m_NCP`$toJSON(),
        self$`m_AP`$toJSON()
      )
    },
    fromJSONString = function(MAutotuneJson) {
      MAutotuneObject <- jsonlite::fromJSON(MAutotuneJson)
      VersionNumberObject <- VersionNumber$new()
      self$`VersionNumber` <- VersionNumberObject$fromJSON(jsonlite::toJSON(MAutotuneObject$VersionNumber, auto_unbox = TRUE))
      self$`m_AutotuningInProgress` <- MAutotuneObject$`m_AutotuningInProgress`
      BigDecimalObject <- BigDecimal$new()
      self$`m_PercentComplete` <- BigDecimalObject$fromJSON(jsonlite::toJSON(MAutotuneObject$m_PercentComplete, auto_unbox = TRUE))
      self$`m_AutotuningSucceeded` <- MAutotuneObject$`m_AutotuningSucceeded`
      self$`m_NumPatternsToAutotune` <- MAutotuneObject$`m_NumPatternsToAutotune`
      self$`m_ErrorStringBuffer` <- MAutotuneObject$`m_ErrorStringBuffer`
      self$`m_FeaturesToTuneArray` <- MAutotuneObject$`m_FeaturesToTuneArray`
      MNCPObject <- MNCP$new()
      self$`m_NCP` <- MNCPObject$fromJSON(jsonlite::toJSON(MAutotuneObject$m_NCP, auto_unbox = TRUE))
      MAPObject <- MAP$new()
      self$`m_AP` <- MAPObject$fromJSON(jsonlite::toJSON(MAutotuneObject$m_AP, auto_unbox = TRUE))
    }
  )
)
