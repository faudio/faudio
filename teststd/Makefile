
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
CXX = xcrun c++ -std=c++0x -stdlib=libc++ -I ../external_libraries/boost/
EXE = ""
else
CXX = c++ -std=c++0x -I ../external_libraries/boost/
EXE = ".exe"
endif


all: lambda unicode

clean:
	rm lambda$(EXE);
	rm unicode$(EXE);

lambda: lambda.cc
	$(CXX) -o lambda$(EXE) lambda.cc && ./lambda

unicode: unicode.cc
	$(CXX) -o unicode$(EXE) unicode.cc && ./unicode