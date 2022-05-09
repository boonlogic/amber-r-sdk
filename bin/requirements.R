packages <- list("httr",
				 "R6",
				 "fs",
				 "rlang",
				 "testthat",
				 "formatR",
				 "devtools",
				 "jsonlite",
				 "iterators",
				 "gms")

for (package in packages) {
	if (!(package %in% installed.packages())) {
		install.packages(package, repos = "https://repo.miserver.it.umich.edu/cran/")
	}
}