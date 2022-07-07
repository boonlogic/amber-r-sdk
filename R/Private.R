# Amber API Server Boon Logic Amber API Server OpenAPI spec version: 1.0.3
#' @title Private operations
#' @description swagger.Default
#'
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'
#' authenticate generates a API token
#'
#'
#' call_api formats the inputs and performs an API call to Amber server
#'
#'
#' convert_to_csv convert data to csv format for API to parse
#'
#'
#' validate_dims check the data for correct formatting
#'
#'
#' is.iterable checks the base layer of data formatting
#'
#' }
Private <- R6::R6Class(
  "Private",
  public = list(
    `user_agent` = "Boon Logic / amber-r-sdk / requests",
    verify = TRUE,
    cert = NULL,
    timeout = 360,
    token = NULL,
    reauthTime = Sys.time(),
    licenseProfile =  NULL,
    authenticate = function() {
      tIn = Sys.time()
      if (self$reauthTime < tIn) {
        queryParams <- c()
        headerParams = c(`Content-Type` = "application/json",
                         `User-Agent` = self$`user_agent`)
        body <- list(`username` = self$licenseProfile$username,
                  `password` = self$licenseProfile$password)

        headers <- httr::add_headers(headerParams)

        urlPath <- "/oauth2"
        resp <- httr::POST(paste0(self$licenseProfile["oauth-server"], urlPath),
                           queryParams, headers, body = body, encode = "json")

        if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
          returnObject <- PostAuth2Response$new()
          returnObject$fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
          self$token <- returnObject$idToken
          if (is.null(self$token)) {
            msg = paste("authentication failed: invalid credentials")
            rlang::abort(msg, class = "AmberCloudError")
          }
          expire_secs = returnObject$`expiresIn`
          if (is.null(expire_secs)) {
            msg = paste("authentication failed: missing expiration")
            rlang::abort(msg, class = "AmberCloudError")
          }
          self$reauthTime <- tIn + strtoi(expire_secs) - 60

        } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 599) {
            msg = httr::content(resp)$message
            rlang::abort(msg, class = "AmberCloudError")
        }

      } else {
        c(TRUE, NULL)
      }

    }, call_api = function(url, method, queryParams, headerParams, body){
        if (Sys.time() > self$reauthTime) {
          self$authenticate()
        }

        headerParams <- append(headerParams, c(`Authorization` = paste0("Bearer ", self$token),
                                               `User-Agent` = self$`user_agent`,
                                               `Content-Type` = "application/json"))
        headers <- httr::add_headers(headerParams)

        if (method == "GET") {
            resp <- httr::GET(url, query = queryParams, headers, httr::timeout(self$timeout))
        }
        else if (method == "POST") {
            resp <- httr::POST(url, query = queryParams, headers, body = body, encode = "json", httr::timeout(self$timeout))
        }
        else if (method == "PUT") {
            resp <- httr::PUT(url, query = queryParams, headers, body = body, encode = "json", httr::timeout(self$timeout))
        }
        else if (method == "DELETE") {
            resp <- httr::DELETE(url, query = queryParams, headers, httr::timeout(self$timeout))
        }
        else {
            stop("http method must be `GET`, `POST`, `PUT` or `DELETE`.")
        }

        if (httr::status_code(resp) < 200 || httr::status_code(resp) >= 300) {
          msg = httr::content(resp)$message
          rlang::abort(msg, class = "AmberCloudError")
        }

        resp
    }, convert_to_csv = function(data) {
        ndim <- self$validate_dims(data)

        if (ndim == 0) {
          data_flat <- list(data)
        } else if (ndim == 1) {
          data_flat <- data
        } else if (ndim == 2) {
          data_flat <- unlist(data)
        }

        for (d in data_flat) {
          if (!is.numeric(d)) {
              # rlang::abort(paste0("contained ", d, " which is not numeric"), class = "ValueError")
              rlang::abort("testing fail")
          }
        }

        paste(lapply(data_flat, function(d) toString(as.numeric(d))), collapse = ",")

    }, validate_dims = function(data) {
        # not-iterable data is a single scalar data point
        if (!self$is.iterable(data)) {
          return(0)
        }

        # iterable and unnested data is a 1-d array
        if (!(TRUE %in% lapply(data, function(x) self$is.iterable(x)))) {
          if (length(data) == 0) {
            rlang::abort("empty", class = "ValueError")
          }
          return(1)
        }

        # iterable and nested data is a 2-d array
        if (FALSE %in% lapply(data, function(x) self$is.iterable(x))) {
          rlang::abort("Cannot mix nested scalars and iterables", class = "ValueError")
        }

        # check for irregular arrays
        subLengths <- lapply(data, function(d) length(d))
        if (length(unique(subLengths)) > 1) {
          rlang::abort("nested sublists must have equal lengths")
        }

        flattened_2d <- unlist(data, recursive = FALSE)

        if (TRUE %in% lapply(flattened_2d, function(x) self$is.iterable(x))) {
          rlang::abort("Cannot be nested deeper than list-of-lists", class = "ValueError")
        }

        if (subLengths[[1]] == 0) {
          rlang::abort("empty", class = "ValueError")
        }

        return(2)

    }, is.iterable = function(x) {
        if (is.character(x)) {
          return(FALSE)
        }

        value <- TRUE
        if (identical(lengths(x), as.integer(1))) {  
          return(FALSE)
        }
        tryCatch({
          iterators::iter(x)
        }, error = function(c) {
          value <<- FALSE
        })

        value
    }
  )
)
