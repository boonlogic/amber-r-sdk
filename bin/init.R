if (!("renv" %in% installed.packages())) {
	install.packages("renv", repos = "https://repo.miserver.it.umich.edu/cran/")
}
renv::init()