# Copyright 2018, Boon Logic Inc

.PHONY: test format format-check clean generate-client docs go-check

init:
	Rscript bin/init.R

format-check: ## check for format errors
	# # formatR::tidy_dir("R", recursive = TRUE)
	Rscript -e 'gms::codeCheck("R", debug = TRUE)'

# clean: ## clean up go cache and modcache
# 	go clean -modcache -cache

generate-client: ## generate amber swagger client code based on json schema file
	swagger-codegen generate -i swagger.json -l r
	Rscript bin/parse_file.R

docs: ## generate documentation
	Rscript -e 'devtools::document(quiet = TRUE)'

build: init
	R CMD build .
	R CMD check BoonAmber*.tar.gz --no-manual

# test-v1, test-v1next, test-dev, test-qa, test-aoc, test-oap
# add additional .license files in test directory to expand / customize tests
# test-%: 
# 	AMBER_TEST_LICENSE_ID=$* go test -timeout 30m -v -coverprofile .coverage.out .
test:
	make test-default

test-%:
	Rscript -e 'renv::restore()' && \
	AMBER_TEST_LICENSE_ID=$* Rscript -e 'devtools::test(reporter = c("summary", "fail"))'

test-local: test-env-check
	Rscript -e 'renv::restore()' && \
	Rscript -e 'devtools::test(reporter = c("summary", "fail"))'

test-env-check:
	@if [[ "${AMBER_TEST_LICENSE_FILE}" == "" || "${AMBER_TEST_LICENSE_ID}" == "" ]]; then \
		echo "AMBER_TEST_LICENSE_FILE and AMBER_TEST_LICENSE_ID are required in environment"; \
		exit 1; \
	fi