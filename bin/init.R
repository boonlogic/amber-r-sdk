if (!("renv" %in% installed.packages())) {
	install.packages("renv", repos = "https://repo.miserver.it.umich.edu/cran/")
}

renv::init()

packages <- list("testthat",
				 "devtools",
				 "tidyverse")

for (package in packages) {
	if (!(package %in% installed.packages())) {
		install.packages(package, repos = "https://repo.miserver.it.umich.edu/cran/")
	}
}

renv::snapshot()