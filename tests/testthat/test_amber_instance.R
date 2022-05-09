saved_env <- list("AMBER_LICENSE_FILE" = NULL,
                  "AMBER_USERNAME" = NULL,
                  "AMBER_PASSWORD" = NULL,
                  "AMBER_SERVER" = NULL,
                  "AMBER_OAUTH_SERVER" = NULL,
                  "AMBER_LICENSE_ID" = NULL,
                  "AMBER_SSL_CERT" = NULL,
                  "AMBER_SSL_VERIFY" = NULL)

clear_environment <- function() {
    for (key in names(saved_env)) {
        if (key %in% names(Sys.getenv())) {
            saved_env[key] <<- Sys.getenv(key)
            Sys.unsetenv(key)
        }
    }
}

restore_environment <- function() {
    for (key in names(saved_env)) {
        if (!is.null(saved_env[[key]])) {
            Sys.setenv(key = saved_env[key])
        } else if (key %in% names(Sys.getenv())) {
            Sys.unsetenv(key)
        }
    }
}

# TEST Amber Instances
test_that("init", {
    clear_environment()

    # load profile using license file specified as parameter
    profile1 <- AmberClient$new(license_file="test.Amber.license")

    # load same profile using environment variable
    tryCatch({
        Sys.setenv("AMBER_LICENSE_FILE" = "test.Amber.license")
        profile2 <- AmberClient$new()
        expect_equal(profile1$license_profile, profile2$license_profile)
    }, error = function(c) {
        expect_false(FALSE, "test for loading the same license_profile from environment failed")
    })

    tryCatch({
        Sys.setenv("AMBER_USERNAME" = "xyyyAmberUser",
                   "AMBER_PASSWORD" = "bogus_password",
                   "AMBER_SERVER" = "https://temp.amber.boonlogic.com/v1",
                   "AMBER_SSL_CERT" = "bogus_ssl_cert",
                   "AMBER_SSL_VERIFY" = "false")
        profile3 <- AmberClient$new(license_file = ".test.Amber.license")
        expect_equal(profile3$license_profile["server"], "https://temp.amber.boonlogic.com/v1")
        expect_equal(profile3$license_profile["username"], "xyyyAmberUser")
        expect_equal(profile3$license_profile["password"], "bogus_password")
        expect_equal(profile3$license_profile["cert"], "bogus_ssl_cert")
        expect_equal(profile3$license_profile["verify"], FALSE)
    }, error = function(c) {
        expect_false(FALSE, "test for override license file through environment failed")
    })
    
    restore_environment()
})

test_that("init_negative", {
    clear_environment()

    # no license file specified
    expect_error(AmberClient$new(license_id = "default",
                                 license_file = "nonexistent-license-file"),
                 class = "AmberUserError")

    # missing required fields
    Sys.setenv("AMBER_LICENSE_FILE" = "test.Amber.license")
    expect_error(AmberClient$new(license_id = "nonexistent-license-id",
                                 license_file = "test.Amber.license"),
                 class = "AmberUserError")
    expect_error(AmberClient$new(license_id = "missing-username",
                                 license_file = "test.Amber.license"),
                 class = "AmberUserError")
    expect_error(AmberClient$new(license_id = "missing-password",
                                 license_file = "test.Amber.license"),
                 class = "AmberUserError")
    expect_error(AmberClient$new(license_id = "missing-server",
                                 license_file = "test.Amber.license"),
                 class = "AmberUserError")
    
    restore_environment()
})