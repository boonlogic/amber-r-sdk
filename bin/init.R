if (!("renv" %in% installed.packages())) {
	install.packages("renv", repos = "https://repo.miserver.it.umich.edu/cran/")
}

renv::init()

packages <- list("devtools",
				 "stringr",
				 "testthat",
		 		 "iterators",
		 		 "base64enc",
		 		 "rjson")
				 # "httr",
		 		#  "jsonlite",
		 		#  "R6",
		 		#  "fs",
		 		#  "rlang")

for (package in packages) {
	if (!(package %in% installed.packages())) {
		# install.packages(package, repos = "https://repo.miserver.it.umich.edu/cran/")
		renv::install(package)
	}
}

renv::snapshot()