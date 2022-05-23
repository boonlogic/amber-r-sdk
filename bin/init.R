if (!("renv" %in% installed.packages())) {
	install.packages("renv", repos = "https://repo.miserver.it.umich.edu/cran/")
}

renv::settings$package.dependency.fields(c("Imports", "Depends", "LinkingTo", "Suggests"))
renv::init()

# packages <- list("testthat",
# 				 "devtools",
# 				 "stringr")
# 				 # "httr",
# 		 		#  "jsonlite",
# 		 		#  "R6",
# 		 		#  "fs",
# 		 		#  "rlang",
# 		 		#  "iterators",
# 		 		#  "botor",
# 		 		#  "base64enc")

# for (package in packages) {
# 	if (!(package %in% installed.packages())) {
# 		# install.packages(package, repos = "https://repo.miserver.it.umich.edu/cran/")
# 		renv::install(package)
# 	}
# }

# renv::snapshot()