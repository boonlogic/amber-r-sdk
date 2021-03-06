# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
#' @title Default operations
#' @description swagger.Default
#'
#' @field license_id Stores url path of the request.
#' @field license_file Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#' delete_sensor Delete a sensor instance
#'
#'
#' get_amber_summary Get the JSON block of the amber image
#'
#'
#' get_config Get the current configuration of a sensor instance
#'
#'
#' get_pretrain_state Get status of pretrain operation
#'
#'
#' get_root_cause Get root cause analysis information from a sensor
#'
#'
#' get_sensor Get basic information about a sensor instance
#'
#'
#' list_sensors List all sensors for this user
#'
#'
#' get_status Get analytic information from a sensor
#'
#'
#' get_version Retrieves API version information
#'
#'
#' configure_sensor Apply configuration to a sensor instance
#'
#'
#' post_oauth2 Request a bearer token using Amber account credentials
#'
#'
#' pretrain_sensor Pretrain a sensor using historical data
#'
#'
#' create_sensor Create a new a sensor instance
#'
#'
#' stream_sensor Stream data to a sensor
#'
#'
#' configure_fusion Update configuration for a sensor instance for fusion data
#'
#'
#' enable_learning set new graduation requirements and turns on learning
#'
#'
#' update_label Update label for a sensor instance
#'
#'
#' stream_fusion Stream data to a sensor fusion vector
#'
#' }
#'
#' @examples
#'
#' amber <- AmberClient("default", "../tests/testthat/test.Amber.license")
#' print(amber$get_version())
#'
#' @export
AmberClient <- R6::R6Class(
  "AmberClient",
  public = list(
    license_id = NULL,
    license_file = NULL,
    restPrivate = NULL,
    #' @param license_id key value for which amber server to use
    #' @param license_file path to the Amber license file containing user credentials and server address
    #' @param verify whether or not to verify the connection
    #' @param cert whether or not to require certification in connect
    #' @param timeout number of seconds to wait before failing connection
    initialize = function(license_id = "default", license_file = "~/.Amber.license", verify = FALSE, cert = NULL, timeout = 60000) {
        private <- Private$new()

        self$license_file <- license_file
        self$license_file <- Sys.getenv("AMBER_LICENSE_FILE")
        if(self$license_file == "") {
          self$license_file <- license_file
        }

        self$license_id <- license_id
        self$license_id <- Sys.getenv("AMBER_LICENSE_ID")
        if(self$license_id == "") {
          self$license_id <- license_id
        }

        if (!is.null(license_file)) {
          license_path <- fs::path_expand(self$license_file)
          if (file.exists(license_path)){
            file_data = NULL
            tryCatch(
              file_data <- rjson::fromJSON(file = license_path, simplify = FALSE),
              error = function(c) {
                msg = paste(cat("JSON formatting error in license file: ", license_path))
                rlang::abort(msg, class = "AmberUserError")
              }
            )

            tryCatch({
              private$licenseProfile <- file_data[[self$license_id]]
            }, error = function(c) {
                msg = paste(cat("license_id ", license_id, " not found in license file"))
                rlang::abort(msg, class = "AmberUserError")
              }
            )
          } else {
            private$licenseProfile <- rjson::fromJSON('{"username": "", "password": "", "server": "", "oauth-server": ""}')
          }
        } else {
          private$licenseProfile <- rjson::fromJSON('{"username": "", "password": "", "server": "", "oauth-server": ""}')
        }

        tryCatch({
            private$licenseProfile$username <- Sys.getenv("AMBER_USERNAME", unset = private$licenseProfile$username)
            private$licenseProfile$password <- Sys.getenv("AMBER_PASSWORD", unset = private$licenseProfile$password)
            private$licenseProfile$server <- Sys.getenv("AMBER_SERVER", unset = private$licenseProfile$server)

            # TODO: check if key is missing in license profile oauth-server
            if (is.null(private$licenseProfile$`oauth-server`) || is.null(private$licenseProfile)) {
              private$licenseProfile$`oauth-server` <- private$licenseProfile$server
            }
            private$licenseProfile$`oauth-server` <- Sys.getenv("AMBER_OAUTH_SERVER", unset = private$licenseProfile$`oauth-server`)
          }, error = function(c) {
            msg = paste("missing field in license file")
            rlang::abort(msg, class = "AmberUserError")
          }
        )

        tryCatch({
            if ("AMBER_SSL_CERT" %in% names(Sys.getenv())) {
              private$licenseProfile$cert <- Sys.getenv("AMBER_SSL_CERT")
            } else {
              private$licenseProfile$cert <- cert
            }
            verify_str = tolower(Sys.getenv("AMBER_SSL_VERIFY", unset = "true"))
            private$licenseProfile$verify <- TRUE # Default
            if (!verify || verify_str == "false") {
              private$licenseProfile$verify <- FALSE
            }
          }, error = function(c) {
            msg = "error with verify or cert"
            rlang::abort(msg, class = "AmberUserError")
          }
        )

        if (!private$licenseProfile$verify) {
          # TODO: set verify to false in request thing
          httr::set_config(httr::config(ssl_verifypeer = FALSE))
        }

        private$timeout <- timeout

        if (private$licenseProfile$username == ""){
          msg = paste("username not found in specified")
          rlang::abort(msg, class = "AmberUserError")
        }
        if (private$licenseProfile$password == ""){
          msg = paste("password not found in specified")
          rlang::abort(msg, class = "AmberUserError")
        }
        if (private$licenseProfile$server == ""){
          msg = paste("server not found in specified")
          rlang::abort(msg, class = "AmberUserError")
        }
        
        self$restPrivate <- private

    }, 
    #' @description List all sensors on current Amber server
    #'
    #' @return list of sensorIDs and labels
    list_sensors = function() {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        urlPath <- "/sensors"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams, body = body)

        returnObject <- GetSensorsResponse$new()
        list_sensors <- returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        sensors = list()
        for (sensor in list_sensors) {
          sensors[[sensor$sensorId]] <- sensor$label
        }
        sensors

    }, 
    #' @description Get the label, ID, and usage info for the given sensor
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #'
    #' @return object with sensor ID, label, and usage
    get_sensor = function(sensor_id) {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        urlPath <- "/sensor"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- GetSensorResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    }, 
    #' @description Initialize a new sensor
    #'
    #' @param label string identifier for the sensor
    #'
    #' @return sensor ID
    create_sensor = function(label = "") {
        queryParams <- list()
        headerParams <- character()
        body <- list(`label` = label)

        urlPath <- "/sensor"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "POST", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PostSensorResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$`sensorId`

    }, 
    #' @description Change the string identifier for the sensor
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param label String label for the sensor
    #'
    #' @return label
    update_label = function(sensor_id, label) {
        queryParams <- list()
        headerParams <- character()
        body <- list(label = label)

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        urlPath <- "/sensor"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "PUT", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PutSensorResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$`label`

    },
    #' @description Set the configuration for the sensor
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param feature_count number of features in the vectors to be clustered
    #' @param streaming_window_size shifting window of concatenated vectors 
    #' @param samples_to_buffer number of samples to use for the autotuning buffer
    #' @param anomaly_history_window length of data to consider in "recent anomaly" analysis (AH)
    #' @param learning_rate_numerator number of new clusters in growth graduation requirement
    #' @param learning_rate_denominator recent indexes for cluster growth graduation requirement
    #' @param learning_max_clusters max number of clusters created
    #' @param learning_max_samples max number of samples to cluster in training
    #' @param features specified min/max values for features (won't autotune on set features)
    #'
    #' @return configuration object with all the values that were set
    configure_sensor = function(sensor_id, feature_count = 1,
                                   streaming_window_size = 25,
                                   samples_to_buffer = 10000,
                                   anomaly_history_window = 10000,
                                   learning_rate_numerator = 10,
                                   learning_rate_denominator = 10000,
                                   learning_max_clusters = 1000,
                                   learning_max_samples = 1000000,
                                   features = NULL,
                                   override_pv = NULL) {
        queryParams <- list()
        headerParams <- character()
        body <- list()

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }
        if (feature_count%%1 != 0 || feature_count <= 0){
          msg = "invalid 'feature_count': must be a positive integer"
          rlang::abort(msg, class = "AmberUserError")
        }
        if (streaming_window_size%%1 != 0 || streaming_window_size <= 0){
          msg = "invalid 'streaming_window_size': must be a positive integer"
          rlang::abort(msg, class = "AmberUserError")
        }

        body["featureCount"] <- feature_count
        body["streamingWindowSize"] <- streaming_window_size
        body["samplesToBuffer"] <- samples_to_buffer
        body["anomalyHistoryWindow"] <- anomaly_history_window
        body["learningRateNumerator"] <- learning_rate_numerator
        body["learningRateDenominator"] <- learning_rate_denominator
        body["learningMaxClusters"] <- learning_max_clusters
        body["learningMaxSamples"] <- learning_max_samples
        
        if (!is.null(features)) {
          featureObject <- FeatureConfig$new()
          featureObject$fromJSON(features)
          body["features"] <- featureObject

        }
        
        if (!is.null(override_pv)) {
          body["percentVariationOverride"] <- override_pv
        }

        urlPath <- "/config"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "POST", queryParams = queryParams, headerParams = headerParams,
            body = body)


        returnObject <- PostConfigResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    },
    #' @description Configure fusion vectors' rules
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param feature_count number of features in the vector
    #' @param features list of objects specifying the submit rule for each feature
    #'
    #' @return fusion config object with the values that were set
    configure_fusion = function(sensor_id, feature_count = 5, features = NULL) {
        queryParams <- list()
        headerParams <- character()

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }
        if (is.null(features)) {
          if (feature_count%%1 != 0 || feature_count <= 0){
            msg = "invalid 'feature_count': must be a positive integer"
            rlang::abort(msg, class = "AmberUserError")
          }
          features <- character()
          for (i in 1:feature_count) {
            features <- append(features, c(labels = "", submitRule = ""))
          }
        }

        body <- list(features = features)
        urlPath <- "/config"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "PUT", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PutConfigResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()$features

    },
    #' @description Set new streaming parameters and turn on learning
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param anomaly_history_window length of data to consider in "recent anomaly" analysis (AH)
    #' @param learning_rate_numerator number of new clusters in growth graduation requirement
    #' @param learning_rate_denominator recent indexes for cluster growth graduation requirement
    #' @param learning_max_clusters max number of clusters created
    #' @param learning_max_samples max number of samples to cluster in training
    #'
    #' @return streaming parameters object with the values that were set
    enable_learning = function(sensor_id, anomaly_history_window = NULL,
                                  learning_rate_numerator = NULL,
                                  learning_rate_denominator = NULL,
                                  learning_max_clusters = NULL,
                                  learning_max_samples = NULL) {
        queryParams <- list()
        headerParams <- character()
        body <- list(streaming = list())

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }
        if (!is.null(anomaly_history_window)) {
          body$streaming <- append(body$streaming, c(anomalyHistoryWindow = anomaly_history_window))
        }
        if (!is.null(learning_rate_numerator)) {
          body$streaming <- append(body$streaming, c(learningRateNumerator = learning_rate_numerator))
        }
        if (!is.null(learning_rate_denominator)) {
          body$streaming <- append(body$streaming, c(learningRateDenominator = learning_rate_denominator))
        }
        if (!is.null(learning_max_clusters)) {
          body$streaming <- append(body$streaming, c(learningMaxClusters = learning_max_clusters))
        }
        if (!is.null(learning_max_samples)) {
          body$streaming <- append(body$streaming, c(learningMaxSamples = learning_max_samples))
        }

        urlPath <- "/config"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "PUT", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PutConfigResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()$streaming

    },
    #' @description Get the configuration for the given sensor
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #'
    #' @return configuration object with all the values that were set
    get_config = function(sensor_id) {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        urlPath <- "/config"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- GetConfigResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    },
    #' @description Delete the given sensor ID
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #'
    delete_sensor = function(sensor_id) {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        urlPath <- "/sensor"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "DELETE", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- Error$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        return(NULL)

    }, 
    #' @description Send data to Amber for processing
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param data list of values to cluster
    #' @param save_image if TRUE then save the amber image between calls
    #'
    #' @return status object with state, progress, overview, etc
    stream_sensor = function(sensor_id, data, save_image = TRUE) {
        queryParams <- list()
        headerParams <- character()
        body <- list()

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }
        tryCatch(
          data_csv <- self$restPrivate$convert_to_csv(data)
        , error = function (c) {
          msg = paste0("invalid data: ", c$message)
          rlang::abort(msg, class = "AmberUserError")
        })

        body["saveImage"] <- save_image
        body["data"] <- data_csv

        urlPath <- "/stream"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "POST", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PostStreamResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    }, 
    #' @description Send data to Amber for processing using sensor fusion
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param vector data vector to cluster
    #' @param submit options are: submit, nosubmit, default
    #'
    #' @return vector and submitRule object that was used
    stream_fusion = function(sensor_id, vector, submit = NULL) {
        queryParams <- list()
        headerParams <- character()

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }
        sopts <- list("submit", "nosubmit", "default")
        if (is.null(submit)) {
          submit <- "default"
        }
        if (!(submit %in% sopts)){
          msg = paste0("'submit' must be one of ", sopts, ", got ", submit)
          rlang::abort(msg, class = "AmberUserError")
        }

        body <- list("vector" = vector, "submitRule" = submit)

        urlPath <- "/stream"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "PUT", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PutStreamResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    },
    #' @description Gets the status of the sensor
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #'
    #' @return overview object with analytics about the model
    get_status = function(sensor_id) {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        urlPath <- "/status"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- GetStatusResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        return_json <- returnObject$toJSON()

        GetStatusResponseObject <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
        return_json$pca <- GetStatusResponseObject$pca
        return_json

    },
    #' @description Gets pretrain status
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #'
    #' @return string specifying what state pretraining is in
    get_pretrain_state = function(sensor_id) {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        urlPath <- "/pretrain"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- GetPretrainResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    },
    #' @description Use historical data to train the sensor
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param data list of values to train the sensor
    #' @param autotune_config if TRUE, then pretraining will end in monitoring, otherwise it will use the give config to train
    #' @param block if TRUE, wait for pretraining to finish (always TRUE for on prem)
    #'
    #' @return string specifying what state pretraining is in
    pretrain_sensor = function(sensor_id, data, autotune_config = TRUE, block = TRUE) {
        queryParams <- list()
        headerParams <- character()
        body <- list()

        if (!missing(sensor_id)) {
            headerParams["sensorId"] <- sensor_id
        }

        data_csv <- self$restPrivate$convert_to_csv(data)
        body["data"] <- data_csv
        body["autotuneConfig"] <- autotune_config

        urlPath <- "/pretrain"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "POST", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- PostPretrainResponse$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        if (!block) {
          returnObject$toJSON()
        } else {
          continue <- TRUE
          while (continue) {
            results <- self$get_pretrain_state(sensor_id)
            if (results$state == "Pretraining") {
              Sys.sleep(5)
            } else {
              continue <- FALSE
              return(results)
            }
          }
        }

    }, 
    #' @description Get the root cause analysis for the cluster ids or the input pattern vectors
    #'
    #' @param sensor_id Boon generate identifier for the sensor
    #' @param cluster_id list of cluster IDs to pull the root cause for
    #' @param pattern list of vectors to generate the root cause for
    #'
    #' @return vector of analytics for the given cluster ID or pattern vector
    get_root_cause = function(sensor_id, cluster_id, pattern_list) {
        queryParams <- list()
        headerParams <- character()

        if (!missing(cluster_id) && !missing(pattern_list)) {
          msg = "cannot specify both pattern_lists and cluster IDs for analysis"
          rlang::abort(msg, class = "AmberUserError")
        }
        if (missing(cluster_id) && missing(pattern_list)) {
          msg = "Must specify either pattern_lists or cluster IDs for analysis"
          rlang::abort(msg, class = "AmberUserError")
        }
        if (!missing(sensor_id)) {
          headerParams["sensorId"] <- sensor_id
        }

        if (!missing(cluster_id)) {
          cluster_id_str <- paste0("[", paste0(cluster_id, collapse = ","), "]")
          queryParams["clusterID"] <- cluster_id_str
        }

        if (!missing(pattern_list)) {
          if (length(lengths(pattern_list)) == 1) { #only 1 pattern provided
            pattern_list = list(pattern_list)
          } else {
            for (i in seq_along(pattern_list)) {
              pattern_list[[i]] <- paste0(pattern_list[[i]], collapse = ",")
            }
            pattern_list_str <- paste0("[[", paste(pattern_list, collapse = "],["), "]]")
          }
          queryParams["pattern"] <- pattern_list_str
        }

        urlPath <- "/rootCause"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams)

        returnObject <- GetRootCauseResponse$new()
        root_cause <- returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        lapply(root_cause, function(c) unlist(c))

    }, 
    #' @description Get the current version numbers for the Amber server
    #'
    #' @return object containing the various version numbers for the parts of Amber
    get_version = function() {
        queryParams <- list()
        headerParams <- character()
        body <- NULL

        urlPath <- "/version"
        resp <- self$restPrivate$call_api(url = paste0(self$restPrivate$licenseProfile$server, urlPath),
            method = "GET", queryParams = queryParams, headerParams = headerParams,
            body = body)

        returnObject <- Version$new()
        returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        returnObject$toJSON()

    }
  )
)
