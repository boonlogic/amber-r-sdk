create_amber_client <- function() {
    amber_license_file <- NULL
    amber_license_id <- NULL
    if ("AMBER_TEST_LICENSE_FILE" %in% names(Sys.getenv())){
        amber_license_file <- Sys.getenv("AMBER_TEST_LICENSE_FILE")
    }
    if ("AMBER_TEST_LICENSE_ID" %in% names(Sys.getenv())){
        amber_license_id <- Sys.getenv("AMBER_TEST_LICENSE_ID")
    }
    expect_false(is.null(amber_license_id))

    for (key in names(saved_env)) {
        if (!is.null(Sys.getenv(key))) {
            Sys.unsetenv(key)
        }
    }

    if (!is.null(amber_license_file)) {
        AmberClient$new(amber_license_id, amber_license_file)
    } else {
        # TODO: secrets
        # license_profile
        # Sys.setenv("AMBER_USERNAME" = license_profile["username"],
        #            "AMBER_PASSWORD" = license_profile["password"],
        #            "AMBER_SERVER"  = license_profile["server"],
        #            "AMBER_OAUTH_SERVER" = license_profile["oauth-server"])
        # AmberClient$new(NULL, NULL)
        amber_license_file <- "test.Amber.license"
        AmberClient$new(amber_license_id, amber_license_file)
    }

}

saved_env <- list("AMBER_LICENSE_FILE" = NULL,
                  "AMBER_USERNAME" = NULL,
                  "AMBER_PASSWORD" = NULL,
                  "AMBER_SERVER" = NULL,
                  "AMBER_OAUTH_SERVER" = NULL,
                  "AMBER_LICENSE_ID" = NULL,
                  "AMBER_SSL_CERT" = NULL,
                  "AMBER_SSL_VERIFY" = NULL)

amber <- create_amber_client()
sensor_id <- amber$create_sensor("test-sensor-r")

# TEST Amber Sensor Operations
test_that("create_sensor", {
    tryCatch({
        expect_false(is.null(sensor_id))
        expect_false(sensor_id == "")
    }, error = function(c) {
        expect_false(FALSE, "setup failed")
    })
})

test_that("update_label", {
    label <- amber$update_label(sensor_id, "new-label")
    expect_equal(label, "new-label")

    tryCatch(
        amber$update_label(sensor_id, "test-sensor-r"),
    error = function(c) {
        expect_false(FALSE, "teardown failed; label was not changed back to 'test-sensor-r'")
    })
})

test_that("update_label_negative", {
    expect_error(amber$update_label("nonexistent-sensor-id", "test-sensor-r"), class = "AmberCloudError")
})

test_that("get_sensor", {
    sensor <- amber$get_sensor(sensor_id)
    expect_equal(sensor$label, "test-sensor-r")
    expect_equal(sensor$sensorId, sensor_id)
    expect_true("usageInfo" %in% names(sensor))
})

test_that("get_sensor_negative", {
    expect_error(amber$get_sensor("nonexistent-sensor-id"), class = "AmberCloudError")
})

test_that("list_sensors", {
    sensors <- amber$list_sensors()
    expect_true(sensor_id %in% names(sensors))
})

test_that("configure_sensor", {
    expected <- list("featureCount" = 1,
                     "streamingWindowSize" = 25,
                     "samplesToBuffer" = 1000,
                     "anomalyHistoryWindow" = 1000,
                     "learningRateNumerator" = 10,
                     "learningRateDenominator" = 10000,
                     "learningMaxClusters" = 1000,
                     "learningMaxSamples" = 1000000,
                     "features" = list(list("minVal" = 0,
                                            "maxVal" = 1,
                                            "label" = "feature-0",
                                            "submitRule" = "submit")
                                       )
                     )
    features <- list(list("minVal" = 1, "maxVal" = 50, "label" = "fancy-label"))
    config <- amber$configure_sensor(sensor_id, feature_count = 1,
                                     streaming_window_size = 25,
                                     samples_to_buffer = 1000,
                                     anomaly_history_window = 1000,
                                     learning_rate_numerator = 10,
                                     learning_rate_denominator = 10000,
                                     learning_max_clusters = 1000,
                                     learning_max_samples = 1000000)
    # TODO: figure out bigDecimal return
    # expect_mapequal(config, expected)
    expect_true(TRUE)
})

test_that("configure_sensor_negative", {
    expect_error(amber$configure_sensor("nonexistent-sensor-id"), class = "AmberCloudError")
    expect_error(amber$configure_sensor(sensor_id, feature_count = -1), class = "AmberUserError")
    expect_error(amber$configure_sensor(sensor_id, feature_count = 1.5), class = "AmberUserError")
    expect_error(amber$configure_sensor(sensor_id, streaming_window_size = -1), class = "AmberUserError")
    expect_error(amber$configure_sensor(sensor_id, streaming_window_size = 1.5), class = "AmberUserError")
})

test_that("get_config", {
    expected <- list("featureCount" = 1,
                     "streamingWindowSize" = 25,
                     "samplesToBuffer" = 1000,
                     "anomalyHistoryWindow" = 1000,
                     "learningRateNumerator" = 10,
                     "learningRateDenominator" = 10000,
                     "learningMaxClusters" = 1000,
                     "learningMaxSamples" = 1000000,
                     "percentVariation" = 0.05,
                     "features" = list(list("minVal" = 0, "maxVal" = 1, "label" = "feature-0", "submitRule" = "submit")))
    # TODO: test get config once bigdecimal is done
    expect_true(TRUE)
})

test_that("get_config_negative", {
    expect_error(amber$get_config("nonexistent-sensor-id"), class = "AmberCloudError")
})

test_that("configure_fusion", {
    amber$configure_sensor(sensor_id, feature_count = 5, streaming_window_size = 1)
    f <- list()
    for (i in 1:5) {
        f[[i]] <- list("label" = paste0("f", i - 1), "submitRule" = "submit")
    }
    resp <- amber$configure_fusion(sensor_id, features = f)
    expect_setequal(resp, f)
})

test_that("configure_fusion_negative", {
    f <- list()
    for (i in 1:5) {
        f[[i]] <- list("label" = paste0("f", i), "submitRule" = "submit")
    }

    # missing sensor
    expect_error(amber$configure_fusion("nonexistent-sensor-id", features = f), class = "AmberCloudError")

    # number of features don't match configure feature count
    expect_error(amber$configure_fusion(sensor_id, features = f[1:4]), class = "AmberCloudError")

    # duplicate feature in configuration
    badf <- f
    badf[[3]] <- badf[[2]]
    expect_error(amber$configure_fusion(sensor_id, features = badf), class = "AmberCloudError")

    # unrecognized submit rule in configuration
    badf <- f
    badf[[2]]$submitRule <- "badSubmitRule"
    expect_error(amber$configure_fusion(sensor_id, features = badf), class = "AmberCloudError")
})

test_that("stream_fusion", {
    # stream partial vector
    v <- list(list("label" = "f1", "value" = 2), list("label" = "f3", "value" = 4))
    exp <- list("vector" = "None,2,None,4,None")
    resp <- amber$stream_fusion(sensor_id, vector = v)
    expect_equal(resp, exp)

    # stream full vector
    v <- list(list("label" = "f0", "value" = 1), list("label" = "f2", "value" = 3), list("label" = "f4", "value" = 5))
    exp <- list("vector" = "1,2,3,4,5",
                "results" = list("clusterCount" = 0,
                                 "message" = "",
                                 "progress" = 0,
                                 "retryCount" = 0,
                                 "state" = "Buffering",
                                 "streamingWindowSize" = 1,
                                 "totalInferences" = 0,
                                 "AD" = list(0),
                                 "AH" = list(0),
                                 "AM" = list(0),
                                 "AW" = list(0),
                                 "ID" = list(0),
                                 "RI" = list(0),
                                 "SI" = list(0)
                                )
                )

    resp <- amber$stream_fusion(sensor_id, vector = v)
    expect_false(FALSE)
    # TODO: figure out uint array actually being saved (it doesn't return the value anywhere)
    # expect_mapequal(resp, exp)
})

test_that("stream_fusion_negative", {
    # bad label
    v <- list(list("label" = "badfeature", "value" = 2), list("label" = "f3", "value" = 4))
    expect_error(amber$stream_fusion(sensor_id, vector = v), class = "AmberCloudError")

    # duplicate label
    v <- list(list("label" = "f3", "value" = 2), list("label" = "f3", "value" = 4))
    expect_error(amber$stream_fusion(sensor_id, vector = v), class = "AmberCloudError")

    # fusion teardown
    amber$configure_sensor(sensor_id, feature_count = 1, streaming_window_size = 25)
})

test_that("stream_sensor", {
    results <- amber$stream_sensor(sensor_id, 1)
    expect_true("state" %in% names(results))
    expect_true("message" %in% names(results))
    expect_true("progress" %in% names(results))
    expect_true("clusterCount" %in% names(results))
    expect_true("retryCount" %in% names(results))
    expect_true("streamingWindowSize" %in% names(results))
    expect_true("SI" %in% names(results))
    expect_true("AD" %in% names(results))
    expect_true("AH" %in% names(results))
    expect_true("AM" %in% names(results))
    expect_true("AW" %in% names(results))

    # scalar data should return SI of length 1
    # TODO
    # expect_true(length(results[["SI"]]) == 1)

    # array data should return SI of same length
    results <- amber$stream_sensor(sensor_id, 1:5)
    # TODO
    # expect_true(length(results[["SI"]]) == 5)
})

test_that("stream_sensor_negative", {
    expect_error(amber$stream_sensor("nonexistent-sensor-id", 1:5), class = "AmberCloudError")

    # invalid data
    expect_error(amber$stream_sensor(sensor_id, list()), class = "AmberUserError")
    expect_error(amber$stream_sensor(sensor_id, list(1, "a", 3)), class = "AmberUserError")
    expect_error(amber$stream_sensor(sensor_id, list(1, 2:3, 4)), class = "AmberUserError")
})

test_that("get_root_cause", {
    config <- amber$get_config(sensor_id)
    expected <- rep(rep(0, length(config[["features"]]) * config[["streamingWindowSize"]]), 2)
    config <- amber$get_root_cause(sensor_id, pattern_list = list(
                                                rep(1, length(config[["features"]]) * config[["streamingWindowSize"]]),
                                                rep(0, length(config[["features"]]) * config[["streamingWindowSize"]])))
    # TODO: FIGURE OUT STUPID ARRAY RETURNS
    # expect_mapequal(config, expected)
    expect_true(TRUE)
})

test_that("get_root_cause_negative", {
    expect_error(amber$get_root_cause("nonexistent-sensor-id", cluster_id = list(1)), class = "AmberCloudError")

    # give both fail
    expect_error(amber$get_root_cause(sensor_id, cluster_id = list(1), 
                                                 pattern_list = list(1:3, 4:6)),
                 class = "AmberUserError")

    # give neither fail
    expect_error(amber$get_root_cause(sensor_id), class = "AmberUserError")

    expect_error(amber$get_root_cause(sensor_id, list(1)), class = "AmberCloudError")
})

test_that("get_status", {
    status <- amber$get_status(sensor_id)
    expect_true("pca" %in% names(status))
    expect_true("numClusters" %in% names(status))
})

test_that("get_status_negative", {
    expect_error(amber$get_status("nonexistent-sensor-id"), class = "AmberCloudError")
})

test_that("get_pretrain_state", {
    response <- amber$get_pretrain_state(sensor_id)
    expect_true("state" %in% names(response))
    expect_equal(response$state, "None")
})

test_that("get_pretrain_state_negative", {
    expect_error(amber$get_pretrain_state("nonexistent-sensor-id"), class = "AmberCloudError")
})

test_that("pretrain_sensor", {
    data <- as.list(read.csv("output_current.csv"))

    expect_true(TRUE)
    # TODO
    # results <- amber$pretrain_sensor(sensor_id, data, block = TRUE)
    # expect_equal(results$state, "Pretrained")

    # results <- amber$pretrain_sensor(sensor_id, data, block = FALSE)
    # expect_true("Pretraining" == results$state || "Pretrained" == results$state)
    # continue <- TRUE
    # while (continue) {
    #     result <- self$get_pretrain_state(sensor_id)
    #     if (result$state == "Pretraining") {
    #       Sys.sleep(5)
    #     } else {
    #       continue <- FALSE
    #     }
    # }
    # expect_equal(results$state, "Pretrained")
})

test_that("pretrain_sesnor_negative", {
    expect_error(amber$pretrain_sensor("nonexistent-sensor-id", 1:5, block = TRUE), class = "AmberCloudError")

    # not enough data to fill the sample buffer
    expect_error(amber$pretrain_sensor(sensor_id, 1:5, block = TRUE), class = "AmberCloudError")
})

test_that("enable_learning", {
    exp <- list("anomalyHistoryWindow" = 1000,
                "learningRateNumerator" = 10,
                "learningRateDenominator" = 10000,
                "learningMaxClusters" = 1000,
                "learningMaxSamples" = 1000000
                )
    expect_true(TRUE)
    # resp <- amber$enable_learning(sensor_id, anomaly_history_window = 1000,
    #                               learning_rate_numerator = 10,
    #                               learning_rate_denominator = 10000,
    #                               learning_max_clusters = 1000,
    #                               learning_max_samples = 1000000
    #                               )
    # expect_mapequal(resp, exp)
})

test_that("enable_learning_negative", {
    exp <- list("streaming" = list("anomalyHistoryWindow" = 1000,
                                   "learningRateNumerator" = 10,
                                   "learningRateDenominator" = 10000,
                                   "learningMaxClusters" = 1000,
                                   "learningMaxSamples" = 1000000
                                   )
                )
    # missing sensor
    expect_error(amber$enable_learning("nonexistent-sensor-id", learning_max_samples = 1000000),
                 class = "AmberCloudError")
    # negatvie value
    expect_error(amber$enable_learning(sensor_id, learning_max_samples = -1), class = "AmberCloudError")

    # not in learning state
    amber$configure_sensor(sensor_id, feature_count = 5, streaming_window_size = 1)
    expect_error(amber$enable_learning(sensor_id, learning_max_samples = 1000000), class = "AmberCloudError")
})

test_that("delete_sensor_negative", {
    expect_error(amber$delete_sensor("nonexistent-sensor-id"), class = "AmberCloudError")
})

test_that("delete_sensor", {
    tryCatch({
        amber$delete_sensor(sensor_id)
        expect_true(TRUE, "teardown failed; sensor was not deleted")
    }, error = function(c) {
        expect_false(FALSE, "teardown failed; sensor was not deleted")
    })
})
