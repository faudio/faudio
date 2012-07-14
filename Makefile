
# Makefile to invoke build/Makefile

BUILD_DIRECTORY = build

all: 
	@pushd $(BUILD_DIRECTORY); make all; popd;

help: 
	@pushd $(BUILD_DIRECTORY); make help; popd;

clean: 
	@pushd $(BUILD_DIRECTORY); make clean; popd;

depend: 
	@pushd $(BUILD_DIRECTORY); make depend; popd;

edit_cache: 
	@pushd $(BUILD_DIRECTORY); make edit_cache; popd;

rebuild_cache: 
	@pushd $(BUILD_DIRECTORY); make rebuild_cache; popd;


foo: 
	@pushd $(BUILD_DIRECTORY); make foo; popd;
