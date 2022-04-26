# Amber API Server
#
# Boon Logic Amber API Server
#
# OpenAPI spec version: 1.0.3
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GetSummaryResponse Class
#'
#' @field MagicNumber 
#' @field VersionNumber 
#' @field m_Nano 
#' @field m_BufferStats 
#' @field m_Autotune 
#' @field m_AutotuningElbowClusterCounts 
#' @field m_AutotuningElbowPVArray 
#' @field m_StreamingParameters 
#' @field m_AmberStatus 
#' @field m_Training 
#' @field m_AnomalyThreshold 
#' @field m_AmberWarningCriticalValue 
#' @field m_AmberAlertCriticalValue 
#' @field m_ErrorStringBuffer 
#' @field m_ClusteringParametersInitialized 
#' @field m_StreamingMode 
#' @field m_StreamingModeStatus 
#' @field m_ModifiedAt 
#' @field m_AnomalyMetricByAnomalyCount 
#' @field m_RecentAnomalyCount 
#' @field m_ResultsIDArray 
#' @field m_TrainingSamples 
#' @field m_RecentSamples 
#' @field m_RecentRawSamples 
#' @field m_RecentTimes 
#' @field m_RecentSIs 
#' @field m_RecentRIs 
#' @field m_RecentADs 
#' @field m_RecentAHs 
#' @field m_RecentIDs 
#' @field m_RecentAMs 
#' @field m_RecentAWs 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GetSummaryResponse <- R6::R6Class(
  'GetSummaryResponse',
  public = list(
    `MagicNumber` = NULL,
    `VersionNumber` = NULL,
    `m_Nano` = NULL,
    `m_BufferStats` = NULL,
    `m_Autotune` = NULL,
    `m_AutotuningElbowClusterCounts` = NULL,
    `m_AutotuningElbowPVArray` = NULL,
    `m_StreamingParameters` = NULL,
    `m_AmberStatus` = NULL,
    `m_Training` = NULL,
    `m_AnomalyThreshold` = NULL,
    `m_AmberWarningCriticalValue` = NULL,
    `m_AmberAlertCriticalValue` = NULL,
    `m_ErrorStringBuffer` = NULL,
    `m_ClusteringParametersInitialized` = NULL,
    `m_StreamingMode` = NULL,
    `m_StreamingModeStatus` = NULL,
    `m_ModifiedAt` = NULL,
    `m_AnomalyMetricByAnomalyCount` = NULL,
    `m_RecentAnomalyCount` = NULL,
    `m_ResultsIDArray` = NULL,
    `m_TrainingSamples` = NULL,
    `m_RecentSamples` = NULL,
    `m_RecentRawSamples` = NULL,
    `m_RecentTimes` = NULL,
    `m_RecentSIs` = NULL,
    `m_RecentRIs` = NULL,
    `m_RecentADs` = NULL,
    `m_RecentAHs` = NULL,
    `m_RecentIDs` = NULL,
    `m_RecentAMs` = NULL,
    `m_RecentAWs` = NULL,
    initialize = function(`MagicNumber`, `VersionNumber`, `m_Nano`, `m_BufferStats`, `m_Autotune`, `m_AutotuningElbowClusterCounts`, `m_AutotuningElbowPVArray`, `m_StreamingParameters`, `m_AmberStatus`, `m_Training`, `m_AnomalyThreshold`, `m_AmberWarningCriticalValue`, `m_AmberAlertCriticalValue`, `m_ErrorStringBuffer`, `m_ClusteringParametersInitialized`, `m_StreamingMode`, `m_StreamingModeStatus`, `m_ModifiedAt`, `m_AnomalyMetricByAnomalyCount`, `m_RecentAnomalyCount`, `m_ResultsIDArray`, `m_TrainingSamples`, `m_RecentSamples`, `m_RecentRawSamples`, `m_RecentTimes`, `m_RecentSIs`, `m_RecentRIs`, `m_RecentADs`, `m_RecentAHs`, `m_RecentIDs`, `m_RecentAMs`, `m_RecentAWs`){
      if (!missing(`MagicNumber`)) {
        stopifnot(R6::is.R6(`MagicNumber`))
        self$`MagicNumber` <- `MagicNumber`
      }
      if (!missing(`VersionNumber`)) {
        stopifnot(R6::is.R6(`VersionNumber`))
        self$`VersionNumber` <- `VersionNumber`
      }
      if (!missing(`m_Nano`)) {
        stopifnot(R6::is.R6(`m_Nano`))
        self$`m_Nano` <- `m_Nano`
      }
      if (!missing(`m_BufferStats`)) {
        stopifnot(R6::is.R6(`m_BufferStats`))
        self$`m_BufferStats` <- `m_BufferStats`
      }
      if (!missing(`m_Autotune`)) {
        stopifnot(R6::is.R6(`m_Autotune`))
        self$`m_Autotune` <- `m_Autotune`
      }
      if (!missing(`m_AutotuningElbowClusterCounts`)) {
        stopifnot(is.list(`m_AutotuningElbowClusterCounts`), length(`m_AutotuningElbowClusterCounts`) != 0)
        lapply(`m_AutotuningElbowClusterCounts`, function(x) stopifnot(is.character(x)))
        self$`m_AutotuningElbowClusterCounts` <- `m_AutotuningElbowClusterCounts`
      }
      if (!missing(`m_AutotuningElbowPVArray`)) {
        stopifnot(is.list(`m_AutotuningElbowPVArray`), length(`m_AutotuningElbowPVArray`) != 0)
        lapply(`m_AutotuningElbowPVArray`, function(x) stopifnot(is.character(x)))
        self$`m_AutotuningElbowPVArray` <- `m_AutotuningElbowPVArray`
      }
      if (!missing(`m_StreamingParameters`)) {
        stopifnot(R6::is.R6(`m_StreamingParameters`))
        self$`m_StreamingParameters` <- `m_StreamingParameters`
      }
      if (!missing(`m_AmberStatus`)) {
        stopifnot(R6::is.R6(`m_AmberStatus`))
        self$`m_AmberStatus` <- `m_AmberStatus`
      }
      if (!missing(`m_Training`)) {
        stopifnot(R6::is.R6(`m_Training`))
        self$`m_Training` <- `m_Training`
      }
      if (!missing(`m_AnomalyThreshold`)) {
        stopifnot(is.numeric(`m_AnomalyThreshold`), length(`m_AnomalyThreshold`) == 1)
        self$`m_AnomalyThreshold` <- `m_AnomalyThreshold`
      }
      if (!missing(`m_AmberWarningCriticalValue`)) {
        stopifnot(is.numeric(`m_AmberWarningCriticalValue`), length(`m_AmberWarningCriticalValue`) == 1)
        self$`m_AmberWarningCriticalValue` <- `m_AmberWarningCriticalValue`
      }
      if (!missing(`m_AmberAlertCriticalValue`)) {
        stopifnot(is.numeric(`m_AmberAlertCriticalValue`), length(`m_AmberAlertCriticalValue`) == 1)
        self$`m_AmberAlertCriticalValue` <- `m_AmberAlertCriticalValue`
      }
      if (!missing(`m_ErrorStringBuffer`)) {
        stopifnot(is.character(`m_ErrorStringBuffer`), length(`m_ErrorStringBuffer`) == 1)
        self$`m_ErrorStringBuffer` <- `m_ErrorStringBuffer`
      }
      if (!missing(`m_ClusteringParametersInitialized`)) {
        self$`m_ClusteringParametersInitialized` <- `m_ClusteringParametersInitialized`
      }
      if (!missing(`m_StreamingMode`)) {
        self$`m_StreamingMode` <- `m_StreamingMode`
      }
      if (!missing(`m_StreamingModeStatus`)) {
        stopifnot(is.numeric(`m_StreamingModeStatus`), length(`m_StreamingModeStatus`) == 1)
        self$`m_StreamingModeStatus` <- `m_StreamingModeStatus`
      }
      if (!missing(`m_ModifiedAt`)) {
        stopifnot(is.numeric(`m_ModifiedAt`), length(`m_ModifiedAt`) == 1)
        self$`m_ModifiedAt` <- `m_ModifiedAt`
      }
      if (!missing(`m_AnomalyMetricByAnomalyCount`)) {
        stopifnot(is.list(`m_AnomalyMetricByAnomalyCount`), length(`m_AnomalyMetricByAnomalyCount`) != 0)
        lapply(`m_AnomalyMetricByAnomalyCount`, function(x) stopifnot(is.character(x)))
        self$`m_AnomalyMetricByAnomalyCount` <- `m_AnomalyMetricByAnomalyCount`
      }
      if (!missing(`m_RecentAnomalyCount`)) {
        stopifnot(is.numeric(`m_RecentAnomalyCount`), length(`m_RecentAnomalyCount`) == 1)
        self$`m_RecentAnomalyCount` <- `m_RecentAnomalyCount`
      }
      if (!missing(`m_ResultsIDArray`)) {
        stopifnot(is.list(`m_ResultsIDArray`), length(`m_ResultsIDArray`) != 0)
        lapply(`m_ResultsIDArray`, function(x) stopifnot(is.character(x)))
        self$`m_ResultsIDArray` <- `m_ResultsIDArray`
      }
      if (!missing(`m_TrainingSamples`)) {
        stopifnot(R6::is.R6(`m_TrainingSamples`))
        self$`m_TrainingSamples` <- `m_TrainingSamples`
      }
      if (!missing(`m_RecentSamples`)) {
        stopifnot(R6::is.R6(`m_RecentSamples`))
        self$`m_RecentSamples` <- `m_RecentSamples`
      }
      if (!missing(`m_RecentRawSamples`)) {
        stopifnot(R6::is.R6(`m_RecentRawSamples`))
        self$`m_RecentRawSamples` <- `m_RecentRawSamples`
      }
      if (!missing(`m_RecentTimes`)) {
        stopifnot(R6::is.R6(`m_RecentTimes`))
        self$`m_RecentTimes` <- `m_RecentTimes`
      }
      if (!missing(`m_RecentSIs`)) {
        stopifnot(R6::is.R6(`m_RecentSIs`))
        self$`m_RecentSIs` <- `m_RecentSIs`
      }
      if (!missing(`m_RecentRIs`)) {
        stopifnot(R6::is.R6(`m_RecentRIs`))
        self$`m_RecentRIs` <- `m_RecentRIs`
      }
      if (!missing(`m_RecentADs`)) {
        stopifnot(R6::is.R6(`m_RecentADs`))
        self$`m_RecentADs` <- `m_RecentADs`
      }
      if (!missing(`m_RecentAHs`)) {
        stopifnot(R6::is.R6(`m_RecentAHs`))
        self$`m_RecentAHs` <- `m_RecentAHs`
      }
      if (!missing(`m_RecentIDs`)) {
        stopifnot(R6::is.R6(`m_RecentIDs`))
        self$`m_RecentIDs` <- `m_RecentIDs`
      }
      if (!missing(`m_RecentAMs`)) {
        stopifnot(R6::is.R6(`m_RecentAMs`))
        self$`m_RecentAMs` <- `m_RecentAMs`
      }
      if (!missing(`m_RecentAWs`)) {
        stopifnot(R6::is.R6(`m_RecentAWs`))
        self$`m_RecentAWs` <- `m_RecentAWs`
      }
    },
    toJSON = function() {
      GetSummaryResponseObject <- list()
      if (!is.null(self$`MagicNumber`)) {
        GetSummaryResponseObject[['MagicNumber']] <- self$`MagicNumber`$toJSON()
      }
      if (!is.null(self$`VersionNumber`)) {
        GetSummaryResponseObject[['VersionNumber']] <- self$`VersionNumber`$toJSON()
      }
      if (!is.null(self$`m_Nano`)) {
        GetSummaryResponseObject[['m_Nano']] <- self$`m_Nano`$toJSON()
      }
      if (!is.null(self$`m_BufferStats`)) {
        GetSummaryResponseObject[['m_BufferStats']] <- self$`m_BufferStats`$toJSON()
      }
      if (!is.null(self$`m_Autotune`)) {
        GetSummaryResponseObject[['m_Autotune']] <- self$`m_Autotune`$toJSON()
      }
      if (!is.null(self$`m_AutotuningElbowClusterCounts`)) {
        GetSummaryResponseObject[['m_AutotuningElbowClusterCounts']] <- self$`m_AutotuningElbowClusterCounts`
      }
      if (!is.null(self$`m_AutotuningElbowPVArray`)) {
        GetSummaryResponseObject[['m_AutotuningElbowPVArray']] <- self$`m_AutotuningElbowPVArray`
      }
      if (!is.null(self$`m_StreamingParameters`)) {
        GetSummaryResponseObject[['m_StreamingParameters']] <- self$`m_StreamingParameters`$toJSON()
      }
      if (!is.null(self$`m_AmberStatus`)) {
        GetSummaryResponseObject[['m_AmberStatus']] <- self$`m_AmberStatus`$toJSON()
      }
      if (!is.null(self$`m_Training`)) {
        GetSummaryResponseObject[['m_Training']] <- self$`m_Training`$toJSON()
      }
      if (!is.null(self$`m_AnomalyThreshold`)) {
        GetSummaryResponseObject[['m_AnomalyThreshold']] <- self$`m_AnomalyThreshold`
      }
      if (!is.null(self$`m_AmberWarningCriticalValue`)) {
        GetSummaryResponseObject[['m_AmberWarningCriticalValue']] <- self$`m_AmberWarningCriticalValue`
      }
      if (!is.null(self$`m_AmberAlertCriticalValue`)) {
        GetSummaryResponseObject[['m_AmberAlertCriticalValue']] <- self$`m_AmberAlertCriticalValue`
      }
      if (!is.null(self$`m_ErrorStringBuffer`)) {
        GetSummaryResponseObject[['m_ErrorStringBuffer']] <- self$`m_ErrorStringBuffer`
      }
      if (!is.null(self$`m_ClusteringParametersInitialized`)) {
        GetSummaryResponseObject[['m_ClusteringParametersInitialized']] <- self$`m_ClusteringParametersInitialized`
      }
      if (!is.null(self$`m_StreamingMode`)) {
        GetSummaryResponseObject[['m_StreamingMode']] <- self$`m_StreamingMode`
      }
      if (!is.null(self$`m_StreamingModeStatus`)) {
        GetSummaryResponseObject[['m_StreamingModeStatus']] <- self$`m_StreamingModeStatus`
      }
      if (!is.null(self$`m_ModifiedAt`)) {
        GetSummaryResponseObject[['m_ModifiedAt']] <- self$`m_ModifiedAt`
      }
      if (!is.null(self$`m_AnomalyMetricByAnomalyCount`)) {
        GetSummaryResponseObject[['m_AnomalyMetricByAnomalyCount']] <- self$`m_AnomalyMetricByAnomalyCount`
      }
      if (!is.null(self$`m_RecentAnomalyCount`)) {
        GetSummaryResponseObject[['m_RecentAnomalyCount']] <- self$`m_RecentAnomalyCount`
      }
      if (!is.null(self$`m_ResultsIDArray`)) {
        GetSummaryResponseObject[['m_ResultsIDArray']] <- self$`m_ResultsIDArray`
      }
      if (!is.null(self$`m_TrainingSamples`)) {
        GetSummaryResponseObject[['m_TrainingSamples']] <- self$`m_TrainingSamples`$toJSON()
      }
      if (!is.null(self$`m_RecentSamples`)) {
        GetSummaryResponseObject[['m_RecentSamples']] <- self$`m_RecentSamples`$toJSON()
      }
      if (!is.null(self$`m_RecentRawSamples`)) {
        GetSummaryResponseObject[['m_RecentRawSamples']] <- self$`m_RecentRawSamples`$toJSON()
      }
      if (!is.null(self$`m_RecentTimes`)) {
        GetSummaryResponseObject[['m_RecentTimes']] <- self$`m_RecentTimes`$toJSON()
      }
      if (!is.null(self$`m_RecentSIs`)) {
        GetSummaryResponseObject[['m_RecentSIs']] <- self$`m_RecentSIs`$toJSON()
      }
      if (!is.null(self$`m_RecentRIs`)) {
        GetSummaryResponseObject[['m_RecentRIs']] <- self$`m_RecentRIs`$toJSON()
      }
      if (!is.null(self$`m_RecentADs`)) {
        GetSummaryResponseObject[['m_RecentADs']] <- self$`m_RecentADs`$toJSON()
      }
      if (!is.null(self$`m_RecentAHs`)) {
        GetSummaryResponseObject[['m_RecentAHs']] <- self$`m_RecentAHs`$toJSON()
      }
      if (!is.null(self$`m_RecentIDs`)) {
        GetSummaryResponseObject[['m_RecentIDs']] <- self$`m_RecentIDs`$toJSON()
      }
      if (!is.null(self$`m_RecentAMs`)) {
        GetSummaryResponseObject[['m_RecentAMs']] <- self$`m_RecentAMs`$toJSON()
      }
      if (!is.null(self$`m_RecentAWs`)) {
        GetSummaryResponseObject[['m_RecentAWs']] <- self$`m_RecentAWs`$toJSON()
      }

      GetSummaryResponseObject
    },
    fromJSON = function(GetSummaryResponseJson) {
      GetSummaryResponseObject <- jsonlite::fromJSON(GetSummaryResponseJson)
      if (!is.null(GetSummaryResponseObject$`MagicNumber`)) {
        MagicNumberObject <- MagicNumber$new()
        MagicNumberObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$MagicNumber, auto_unbox = TRUE))
        self$`MagicNumber` <- MagicNumberObject
      }
      if (!is.null(GetSummaryResponseObject$`VersionNumber`)) {
        VersionNumberObject <- VersionNumber$new()
        VersionNumberObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$VersionNumber, auto_unbox = TRUE))
        self$`VersionNumber` <- VersionNumberObject
      }
      if (!is.null(GetSummaryResponseObject$`m_Nano`)) {
        m_NanoObject <- MNano$new()
        m_NanoObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_Nano, auto_unbox = TRUE))
        self$`m_Nano` <- m_NanoObject
      }
      if (!is.null(GetSummaryResponseObject$`m_BufferStats`)) {
        m_BufferStatsObject <- MBufferStats$new()
        m_BufferStatsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_BufferStats, auto_unbox = TRUE))
        self$`m_BufferStats` <- m_BufferStatsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_Autotune`)) {
        m_AutotuneObject <- MAutotune$new()
        m_AutotuneObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_Autotune, auto_unbox = TRUE))
        self$`m_Autotune` <- m_AutotuneObject
      }
      if (!is.null(GetSummaryResponseObject$`m_AutotuningElbowClusterCounts`)) {
        self$`m_AutotuningElbowClusterCounts` <- GetSummaryResponseObject$`m_AutotuningElbowClusterCounts`
      }
      if (!is.null(GetSummaryResponseObject$`m_AutotuningElbowPVArray`)) {
        self$`m_AutotuningElbowPVArray` <- GetSummaryResponseObject$`m_AutotuningElbowPVArray`
      }
      if (!is.null(GetSummaryResponseObject$`m_StreamingParameters`)) {
        m_StreamingParametersObject <- MStreamingParameters$new()
        m_StreamingParametersObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_StreamingParameters, auto_unbox = TRUE))
        self$`m_StreamingParameters` <- m_StreamingParametersObject
      }
      if (!is.null(GetSummaryResponseObject$`m_AmberStatus`)) {
        m_AmberStatusObject <- MAmberStatus$new()
        m_AmberStatusObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_AmberStatus, auto_unbox = TRUE))
        self$`m_AmberStatus` <- m_AmberStatusObject
      }
      if (!is.null(GetSummaryResponseObject$`m_Training`)) {
        m_TrainingObject <- MTraining$new()
        m_TrainingObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_Training, auto_unbox = TRUE))
        self$`m_Training` <- m_TrainingObject
      }
      if (!is.null(GetSummaryResponseObject$`m_AnomalyThreshold`)) {
        self$`m_AnomalyThreshold` <- GetSummaryResponseObject$`m_AnomalyThreshold`
      }
      if (!is.null(GetSummaryResponseObject$`m_AmberWarningCriticalValue`)) {
        self$`m_AmberWarningCriticalValue` <- GetSummaryResponseObject$`m_AmberWarningCriticalValue`
      }
      if (!is.null(GetSummaryResponseObject$`m_AmberAlertCriticalValue`)) {
        self$`m_AmberAlertCriticalValue` <- GetSummaryResponseObject$`m_AmberAlertCriticalValue`
      }
      if (!is.null(GetSummaryResponseObject$`m_ErrorStringBuffer`)) {
        self$`m_ErrorStringBuffer` <- GetSummaryResponseObject$`m_ErrorStringBuffer`
      }
      if (!is.null(GetSummaryResponseObject$`m_ClusteringParametersInitialized`)) {
        self$`m_ClusteringParametersInitialized` <- GetSummaryResponseObject$`m_ClusteringParametersInitialized`
      }
      if (!is.null(GetSummaryResponseObject$`m_StreamingMode`)) {
        self$`m_StreamingMode` <- GetSummaryResponseObject$`m_StreamingMode`
      }
      if (!is.null(GetSummaryResponseObject$`m_StreamingModeStatus`)) {
        self$`m_StreamingModeStatus` <- GetSummaryResponseObject$`m_StreamingModeStatus`
      }
      if (!is.null(GetSummaryResponseObject$`m_ModifiedAt`)) {
        self$`m_ModifiedAt` <- GetSummaryResponseObject$`m_ModifiedAt`
      }
      if (!is.null(GetSummaryResponseObject$`m_AnomalyMetricByAnomalyCount`)) {
        self$`m_AnomalyMetricByAnomalyCount` <- GetSummaryResponseObject$`m_AnomalyMetricByAnomalyCount`
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentAnomalyCount`)) {
        self$`m_RecentAnomalyCount` <- GetSummaryResponseObject$`m_RecentAnomalyCount`
      }
      if (!is.null(GetSummaryResponseObject$`m_ResultsIDArray`)) {
        self$`m_ResultsIDArray` <- GetSummaryResponseObject$`m_ResultsIDArray`
      }
      if (!is.null(GetSummaryResponseObject$`m_TrainingSamples`)) {
        m_TrainingSamplesObject <- MRecentSamples$new()
        m_TrainingSamplesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_TrainingSamples, auto_unbox = TRUE))
        self$`m_TrainingSamples` <- m_TrainingSamplesObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentSamples`)) {
        m_RecentSamplesObject <- MRecentSamples$new()
        m_RecentSamplesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentSamples, auto_unbox = TRUE))
        self$`m_RecentSamples` <- m_RecentSamplesObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentRawSamples`)) {
        m_RecentRawSamplesObject <- MRecentSamples$new()
        m_RecentRawSamplesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentRawSamples, auto_unbox = TRUE))
        self$`m_RecentRawSamples` <- m_RecentRawSamplesObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentTimes`)) {
        m_RecentTimesObject <- MRecentTimes$new()
        m_RecentTimesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentTimes, auto_unbox = TRUE))
        self$`m_RecentTimes` <- m_RecentTimesObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentSIs`)) {
        m_RecentSIsObject <- MRecentAnalytics$new()
        m_RecentSIsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentSIs, auto_unbox = TRUE))
        self$`m_RecentSIs` <- m_RecentSIsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentRIs`)) {
        m_RecentRIsObject <- MRecentAnalytics$new()
        m_RecentRIsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentRIs, auto_unbox = TRUE))
        self$`m_RecentRIs` <- m_RecentRIsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentADs`)) {
        m_RecentADsObject <- MRecentAnalytics$new()
        m_RecentADsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentADs, auto_unbox = TRUE))
        self$`m_RecentADs` <- m_RecentADsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentAHs`)) {
        m_RecentAHsObject <- MRecentAnalytics$new()
        m_RecentAHsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentAHs, auto_unbox = TRUE))
        self$`m_RecentAHs` <- m_RecentAHsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentIDs`)) {
        m_RecentIDsObject <- MRecentIDs$new()
        m_RecentIDsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentIDs, auto_unbox = TRUE))
        self$`m_RecentIDs` <- m_RecentIDsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentAMs`)) {
        m_RecentAMsObject <- MRecentAMs$new()
        m_RecentAMsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentAMs, auto_unbox = TRUE))
        self$`m_RecentAMs` <- m_RecentAMsObject
      }
      if (!is.null(GetSummaryResponseObject$`m_RecentAWs`)) {
        m_RecentAWsObject <- MRecentAnalytics$new()
        m_RecentAWsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentAWs, auto_unbox = TRUE))
        self$`m_RecentAWs` <- m_RecentAWsObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "MagicNumber": %s,
           "VersionNumber": %s,
           "m_Nano": %s,
           "m_BufferStats": %s,
           "m_Autotune": %s,
           "m_AutotuningElbowClusterCounts": [%s],
           "m_AutotuningElbowPVArray": [%s],
           "m_StreamingParameters": %s,
           "m_AmberStatus": %s,
           "m_Training": %s,
           "m_AnomalyThreshold": %d,
           "m_AmberWarningCriticalValue": %d,
           "m_AmberAlertCriticalValue": %d,
           "m_ErrorStringBuffer": %s,
           "m_ClusteringParametersInitialized": %s,
           "m_StreamingMode": %s,
           "m_StreamingModeStatus": %d,
           "m_ModifiedAt": %d,
           "m_AnomalyMetricByAnomalyCount": [%s],
           "m_RecentAnomalyCount": %d,
           "m_ResultsIDArray": [%s],
           "m_TrainingSamples": %s,
           "m_RecentSamples": %s,
           "m_RecentRawSamples": %s,
           "m_RecentTimes": %s,
           "m_RecentSIs": %s,
           "m_RecentRIs": %s,
           "m_RecentADs": %s,
           "m_RecentAHs": %s,
           "m_RecentIDs": %s,
           "m_RecentAMs": %s,
           "m_RecentAWs": %s
        }',
        self$`MagicNumber`$toJSON(),
        self$`VersionNumber`$toJSON(),
        self$`m_Nano`$toJSON(),
        self$`m_BufferStats`$toJSON(),
        self$`m_Autotune`$toJSON(),
        lapply(self$`m_AutotuningElbowClusterCounts`, function(x) paste(paste0('"', x, '"'), sep=",")),
        lapply(self$`m_AutotuningElbowPVArray`, function(x) paste(paste0('"', x, '"'), sep=",")),
        self$`m_StreamingParameters`$toJSON(),
        self$`m_AmberStatus`$toJSON(),
        self$`m_Training`$toJSON(),
        self$`m_AnomalyThreshold`,
        self$`m_AmberWarningCriticalValue`,
        self$`m_AmberAlertCriticalValue`,
        self$`m_ErrorStringBuffer`,
        self$`m_ClusteringParametersInitialized`,
        self$`m_StreamingMode`,
        self$`m_StreamingModeStatus`,
        self$`m_ModifiedAt`,
        lapply(self$`m_AnomalyMetricByAnomalyCount`, function(x) paste(paste0('"', x, '"'), sep=",")),
        self$`m_RecentAnomalyCount`,
        lapply(self$`m_ResultsIDArray`, function(x) paste(paste0('"', x, '"'), sep=",")),
        self$`m_TrainingSamples`$toJSON(),
        self$`m_RecentSamples`$toJSON(),
        self$`m_RecentRawSamples`$toJSON(),
        self$`m_RecentTimes`$toJSON(),
        self$`m_RecentSIs`$toJSON(),
        self$`m_RecentRIs`$toJSON(),
        self$`m_RecentADs`$toJSON(),
        self$`m_RecentAHs`$toJSON(),
        self$`m_RecentIDs`$toJSON(),
        self$`m_RecentAMs`$toJSON(),
        self$`m_RecentAWs`$toJSON()
      )
    },
    fromJSONString = function(GetSummaryResponseJson) {
      GetSummaryResponseObject <- jsonlite::fromJSON(GetSummaryResponseJson)
      MagicNumberObject <- MagicNumber$new()
      self$`MagicNumber` <- MagicNumberObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$MagicNumber, auto_unbox = TRUE))
      VersionNumberObject <- VersionNumber$new()
      self$`VersionNumber` <- VersionNumberObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$VersionNumber, auto_unbox = TRUE))
      MNanoObject <- MNano$new()
      self$`m_Nano` <- MNanoObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_Nano, auto_unbox = TRUE))
      MBufferStatsObject <- MBufferStats$new()
      self$`m_BufferStats` <- MBufferStatsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_BufferStats, auto_unbox = TRUE))
      MAutotuneObject <- MAutotune$new()
      self$`m_Autotune` <- MAutotuneObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_Autotune, auto_unbox = TRUE))
      self$`m_AutotuningElbowClusterCounts` <- GetSummaryResponseObject$`m_AutotuningElbowClusterCounts`
      self$`m_AutotuningElbowPVArray` <- GetSummaryResponseObject$`m_AutotuningElbowPVArray`
      MStreamingParametersObject <- MStreamingParameters$new()
      self$`m_StreamingParameters` <- MStreamingParametersObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_StreamingParameters, auto_unbox = TRUE))
      MAmberStatusObject <- MAmberStatus$new()
      self$`m_AmberStatus` <- MAmberStatusObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_AmberStatus, auto_unbox = TRUE))
      MTrainingObject <- MTraining$new()
      self$`m_Training` <- MTrainingObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_Training, auto_unbox = TRUE))
      self$`m_AnomalyThreshold` <- GetSummaryResponseObject$`m_AnomalyThreshold`
      self$`m_AmberWarningCriticalValue` <- GetSummaryResponseObject$`m_AmberWarningCriticalValue`
      self$`m_AmberAlertCriticalValue` <- GetSummaryResponseObject$`m_AmberAlertCriticalValue`
      self$`m_ErrorStringBuffer` <- GetSummaryResponseObject$`m_ErrorStringBuffer`
      self$`m_ClusteringParametersInitialized` <- GetSummaryResponseObject$`m_ClusteringParametersInitialized`
      self$`m_StreamingMode` <- GetSummaryResponseObject$`m_StreamingMode`
      self$`m_StreamingModeStatus` <- GetSummaryResponseObject$`m_StreamingModeStatus`
      self$`m_ModifiedAt` <- GetSummaryResponseObject$`m_ModifiedAt`
      self$`m_AnomalyMetricByAnomalyCount` <- GetSummaryResponseObject$`m_AnomalyMetricByAnomalyCount`
      self$`m_RecentAnomalyCount` <- GetSummaryResponseObject$`m_RecentAnomalyCount`
      self$`m_ResultsIDArray` <- GetSummaryResponseObject$`m_ResultsIDArray`
      MRecentSamplesObject <- MRecentSamples$new()
      self$`m_TrainingSamples` <- MRecentSamplesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_TrainingSamples, auto_unbox = TRUE))
      MRecentSamplesObject <- MRecentSamples$new()
      self$`m_RecentSamples` <- MRecentSamplesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentSamples, auto_unbox = TRUE))
      MRecentSamplesObject <- MRecentSamples$new()
      self$`m_RecentRawSamples` <- MRecentSamplesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentRawSamples, auto_unbox = TRUE))
      MRecentTimesObject <- MRecentTimes$new()
      self$`m_RecentTimes` <- MRecentTimesObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentTimes, auto_unbox = TRUE))
      MRecentAnalyticsObject <- MRecentAnalytics$new()
      self$`m_RecentSIs` <- MRecentAnalyticsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentSIs, auto_unbox = TRUE))
      MRecentAnalyticsObject <- MRecentAnalytics$new()
      self$`m_RecentRIs` <- MRecentAnalyticsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentRIs, auto_unbox = TRUE))
      MRecentAnalyticsObject <- MRecentAnalytics$new()
      self$`m_RecentADs` <- MRecentAnalyticsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentADs, auto_unbox = TRUE))
      MRecentAnalyticsObject <- MRecentAnalytics$new()
      self$`m_RecentAHs` <- MRecentAnalyticsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentAHs, auto_unbox = TRUE))
      MRecentIDsObject <- MRecentIDs$new()
      self$`m_RecentIDs` <- MRecentIDsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentIDs, auto_unbox = TRUE))
      MRecentAMsObject <- MRecentAMs$new()
      self$`m_RecentAMs` <- MRecentAMsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentAMs, auto_unbox = TRUE))
      MRecentAnalyticsObject <- MRecentAnalytics$new()
      self$`m_RecentAWs` <- MRecentAnalyticsObject$fromJSON(jsonlite::toJSON(GetSummaryResponseObject$m_RecentAWs, auto_unbox = TRUE))
    }
  )
)