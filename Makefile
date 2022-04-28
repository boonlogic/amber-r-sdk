# Copyright 2018, Boon Logic Inc

.PHONY: test format format-check clean generate-client docs go-check

format-check: ## check for format errors
	RScript bin/format.R

# clean: ## clean up go cache and modcache
# 	go clean -modcache -cache

generate-client: ## generate amber swagger client code based on json schema file
	swagger-codegen generate -i swagger.json -l r

docs: ## generate documentation
	RScript bin/gen-docs.R

compile:
	R CMD build .
	R CMD check BoonAmber*.tar.gz --no-manual

# test-v1, test-v1next, test-dev, test-qa, test-aoc, test-oap
# add additional .license files in test directory to expand / customize tests
# test-%: 
# 	AMBER_TEST_LICENSE_ID=$* go test -timeout 30m -v -coverprofile .coverage.out .

init:
	RScript requirements.R
