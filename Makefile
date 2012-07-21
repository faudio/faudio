
# Makefile to invoke build/Makefile

BUILD_DIRECTORY = build

.PHONY: all
all: 
	@pushd $(BUILD_DIRECTORY); make all; popd;

.PHONY: help
help: 
	@pushd $(BUILD_DIRECTORY); make help; popd;

.PHONY: test
test: 
	@pushd $(BUILD_DIRECTORY); make test; popd;

.PHONY: clean
clean: 
	@pushd $(BUILD_DIRECTORY); make clean; popd;

.PHONY: depend
depend: 
	@pushd $(BUILD_DIRECTORY); make depend; popd;

.PHONY: edit_cache
edit_cache: 
	@pushd $(BUILD_DIRECTORY); make edit_cache; popd;

.PHONY: rebuild_cache
rebuild_cache: 
	@pushd $(BUILD_DIRECTORY); make rebuild_cache; popd;

.PHONY: components_print
components_print: 
	@pushd $(BUILD_DIRECTORY); make components_print; popd;

.PHONY: components_resolve
components_resolve: 
	@pushd $(BUILD_DIRECTORY); make components_resolve; popd;

.PHONY: components_clean
components_clean: 
	@pushd $(BUILD_DIRECTORY); make components_clean; popd;

