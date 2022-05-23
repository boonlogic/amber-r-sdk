# library("botor")
library("base64enc")

get_secrets = function() {
	secrete_name <- "amber-test-users"
	region_name <- "us-east-1"

	session <- botor::botor(region_name = region_name)
	client <- session$client(service_name = "secretsmanager")
	print(names(client))

	tryCatch({
		get_secret_value_response <- client$get_secret_value(SecretId = secret_name)
	}, error = function(c) {
		if (c$response$Error$Code == "DecryptionFailureException") {
			rlang::abort(c)
		} else if (c$response$Error$Code == "InternalServiceErrorException") {
			rlang::abort(c)
		} else if (c$response$Error$Code == "InvalidParameterException") {
			rlang::abort(c)
		} else if (c$response$Error$Code == "InvalidRequestException") {
			rlang::abort(c)
		} else if (c$response$Error$Code == "ResourceNotFoundException") {
			rlang::abort(c)
		} else {
			if ("SecretString" %in% names(get_secret_value_response)) {
				secret <- get_secret_value_response$SecretString
			} else {
				decoded_binary_secret <- base64enc::base64decode(get_secret_value_response$SecretBinary)
			}
		}
	})
}