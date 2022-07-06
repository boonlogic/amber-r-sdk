get_secrets = function() {
	secret_name <- "amber-test-users"
	region_name <- "us-east-1"

	# session <- botor::botor(region_name = region_name)
	# client <- session$client(service_name = "secretsmanager")

	# tryCatch({
	# 	get_secret_value_response <- client$get_secret_value(SecretId = secret_name)
	# }, error = function(c) {
	# 	if (c$response$Error$Code == "DecryptionFailureException") {
	# 		rlang::abort(c)
	# 	} else if (c$response$Error$Code == "InternalServiceErrorException") {
	# 		rlang::abort(c)
	# 	} else if (c$response$Error$Code == "InvalidParameterException") {
	# 		rlang::abort(c)
	# 	} else if (c$response$Error$Code == "InvalidRequestException") {
	# 		rlang::abort(c)
	# 	} else if (c$response$Error$Code == "ResourceNotFoundException") {
	# 		rlang::abort(c)
	# 	}
	# })

	get_secret_value_response <- try(system(paste("aws secretsmanager get-secret-value --secret-id", secret_name), intern = TRUE))
	get_secret_value_response <- rjson::fromJSON(paste0(get_secret_value_response, collapse="\n"))
	if ("SecretString" %in% names(get_secret_value_response)) {
		secret <- get_secret_value_response$SecretString
	} else {
		secret <- base64enc::base64decode(get_secret_value_response$SecretBinary)
	}
	rjson::fromJSON(secret)
}