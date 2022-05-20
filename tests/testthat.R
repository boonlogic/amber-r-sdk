if (!("renv" %in% installed.packages())) {
	install.packages("testthat", repos = "https://repo.miserver.it.umich.edu/cran/")
}

library(testthat)
library(BoonAmber)

Sys.setenv("AMBER_TEST_LICENSE_ID" = "default")

test_check("BoonAmber")
