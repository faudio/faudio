
# Makefile to invoke build/Makefile

BUILD_DIRECTORY = build
MAKE			= make --no-print-directory

.PHONY: all
all: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) all; popd;

.PHONY: help
help: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) help; popd;

.PHONY: test
test: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) test; popd;

.PHONY: clean
clean: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) clean; popd;

.PHONY: depend
depend: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) depend; popd;

.PHONY: modules
modules: 
	make -f modules/Makefile;

.PHONY: edit_cache
edit_cache: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) edit_cache; popd;

.PHONY: rebuild_cache
rebuild_cache: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) rebuild_cache; popd;

.PHONY: components_print
components_print: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) components_print; popd;

.PHONY: components_resolve
components_resolve: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) components_resolve; popd;

.PHONY: components_clean
components_clean: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) components_clean; popd;

.PHONY: format                      
DIRS=scl scl-c
format: 
	./cmake/scripts/astyle.sh $(DIRS)

.PHONY: doc
doc: 
	./cmake/scripts/doc.sh

.PHONY: run_scorecleaner
run_scorecleaner: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) run_scorecleaner; popd;

