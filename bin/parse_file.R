if (!("renv" %in% installed.packages())) {
	install.packages("stringr", repos = "https://repo.miserver.it.umich.edu/cran/")
}
library(stringr)

fileName <- "R/EndpointUsageInfo.R"

for (fileName in list.files("R", full.names = TRUE)) {
	if (fileName == "R/sdk.r") {
		f <- readChar("NAMESPACE", file.info("NAMESPACE")$size)
		f_split <- str_split(f, "\n", simplify = TRUE)
		request_bool <- str_detect(f_split, "Request")
		request_str <- f_split[which(request_bool)]

		comment_bool <- str_detect(f_split, "\\#")
		comment_str <- f_split[which(comment_bool)]

		f <- paste0(paste0(comment_str, collapse="\n"), "\n\n", paste0(request_str, collapse="\n"))

		if (!grepl(f, "AmberClient", fixed = TRUE)) {
			f <- paste0(f, "\n\nexport(AmberClient)\n")
			f <- paste0(f, "\n",
						   "import(httr)\nimportFrom(jsonlite, fromJSON)\nimportFrom(jsonlite, toJSON)\nimportFrom(R6, R6Class)\n",
						   "importFrom(fs, path_expand)\nimportFrom(rlang, abort)\n")
		}

		writeLines(f, "NAMESPACE")
		next
	}
	f <- readChar(fileName, file.info(fileName)$size)
	f_split <- str_split(f, "\n", simplify = TRUE)
	write_file <- FALSE
	indeces_to_remove <- c()

	# jsonlite::toJSON locations needed for uint array and big decimal a=objects
	json_lite_bool <- str_detect(f_split, "jsonlite::toJSON\\(")
	loc <- str_locate_all(f_split[which(json_lite_bool)], "jsonlite::toJSON\\(")
	loc <- unlist(lapply(loc, function(c) c[2]))
	json_lite_loc <- which(json_lite_bool)

	# BIG DECIMAL
	indeces_to_remove <- append(indeces_to_remove, which(str_detect(f_split, "BigDecimal\\$")))
	if (length(which(str_detect(f_split, "BigDecimal\\$"))) != 0) {
		write_file <- TRUE

		for (i in seq_along(json_lite_loc)) {
			s <- f_split[json_lite_loc[i]]
			len <- length(unlist(str_locate_all(s, "\\$")))
			was_big_decimal <- FALSE

			# fromJSONString
			if (str_detect(s, "BigDecimalObject\\$") && len == 6) {
				was_big_decimal <- TRUE

				first <- loc[i] + 1
				last <- unlist(str_locate_all(s, ","))[length(unlist(str_locate_all(s, ",")))] - 1
				object_var <- substr(s, first, last)
				var <- str_split(object_var, "\\$", simplify = TRUE)[2]

				first <- str_locate(s, "BigDecimalObject\\$")[1]
				f_split[json_lite_loc[i]] <- paste0(substr(s, 1, first - 1), object_var)
			# fromJSON
			} else if (str_detect(f_split[json_lite_loc[i]-1], "BigDecimal\\$") && len == 4) {
				was_big_decimal <- TRUE

				first <- loc[i] + 1
				last <- unlist(str_locate_all(s, ","))[length(unlist(str_locate_all(s, ",")))] - 1
				object_var <- substr(s, first, last)
				var <- str_split(object_var, "\\$", simplify = TRUE)[2]

				s <- f_split[json_lite_loc[i]+1]
				first <- str_locate(s, "<-")[2]
				indeces_to_remove <<- append(indeces_to_remove, json_lite_loc[i])
				f_split[json_lite_loc[i]+1] <- paste0(substr(s, 1, first + 1), object_var)
			}

			if (was_big_decimal) {
				for (i in which(str_detect(f_split, paste0(var, "`\\$toJSON\\(\\)")))) {
					bounds <- str_locate(f_split[i], paste0(var, "`\\$toJSON\\(\\)"))
					f_split[i] <- paste0(substr(f_split[i], 1, bounds[2] - nchar("$toJSON()")),
										 substr(f_split[i], bounds[2] + 1, nchar(f_split[i])))
				}
			}
		}
	}

	# ARRAY OBJECT
	indeces_to_remove <- append(indeces_to_remove, which(str_detect(f_split, "Array\\$new\\(\\)")))
	if (length(which(str_detect(f_split, "Array\\$new\\(\\)"))) != 0) {
		write_file <- TRUE

		for (i in seq_along(json_lite_loc)) {
			s <- f_split[json_lite_loc[i]]
			len <- length(unlist(str_locate_all(s, "\\$")))
			was_array <- FALSE

			# fromJSONString
			if (str_detect(s, "ArrayObject\\$") && len == 6) {
				was_array <- TRUE

				first <- loc[i] + 1
				last <- unlist(str_locate_all(s, ","))[length(unlist(str_locate_all(s, ",")))] - 1
				object_var <- substr(s, first, last)
				var <- str_split(object_var, "\\$", simplify = TRUE)[2]

				first <- str_locate(s, "<- ")[2]
				f_split[json_lite_loc[i]] <- paste0(substr(s, 1, first), object_var)
			# fromJSON
			} else if (str_detect(f_split[json_lite_loc[i]-1], "Array\\$") && len == 4) {
				was_array <- TRUE

				first <- loc[i] + 1
				last <- unlist(str_locate_all(s, ","))[length(unlist(str_locate_all(s, ",")))] - 1
				object_var <- substr(s, first, last)
				var <- str_split(object_var, "\\$", simplify = TRUE)[2]

				s <- f_split[json_lite_loc[i]+1]
				first <- str_locate(s, "<-")[2]
				indeces_to_remove <<- append(indeces_to_remove, json_lite_loc[i])
				f_split[json_lite_loc[i]+1] <- paste0(substr(s, 1, first + 1), object_var)
			}

			if (was_array) {
				for (i in which(str_detect(f_split, paste0(var, "`\\$toJSON\\(\\)")))) {
					bounds <- str_locate(f_split[i], paste0(var, "`\\$toJSON\\(\\)"))
					f_split[i] <- paste0(substr(f_split[i], 1, bounds[2] - nchar("$toJSON()")),
										 substr(f_split[i], bounds[2] + 1, nchar(f_split[i])))
				}
			}
		}
	}

	if (write_file) {
		f_split <- f_split[-indeces_to_remove]
	}

	# SIMPLIFYVECTOR = FALSE
	json_lite_bool <- str_detect(f_split, "jsonlite::fromJSON\\(")
	for (i in which(json_lite_bool)) {
		write_file <- TRUE
		f_split[i] <- str_replace(f_split[i], "\\)", ", simplifyVector = FALSE)")
	}

	if (write_file) {
		writeLines(f_split, fileName)
	}
}
