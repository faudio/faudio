
# Makefile to invoke build/Makefile

BUILD_DIRECTORY = build
DEBUGGER	= lldb
MAKE		= make --no-print-directory

.PHONY: all
all: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) all; popd;

.PHONY: fresh
fresh:  clean all

.PHONY: help
help: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) help; popd;

.PHONY: clean
clean: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) clean; popd;

.PHONY: depend
depend: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) depend; popd;

.PHONY: modules
modules: 
	make -f modules/Makefile;

.PHONY: bindings
bindings: 
	make -f modules/Makefile bindings;

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
format:
	astyle -n -r "src/*.c" "src/*.d" "src/*.h" "test/*.c"

.PHONY: doc
doc: 
	make -f doc/Makefile

.PHONY: test
test: all
	echo && \
	build/bin/doremir_audio_tests;

.PHONY: debug
debug: all
	$(DEBUGGER) build/bin/doremir_audio_tests;

.PHONY: run_scorecleaner
run_scorecleaner: 
	@pushd $(BUILD_DIRECTORY); $(MAKE) run_scorecleaner; popd;

