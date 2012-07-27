
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
CXX = xcrun c++ -stdlib=libc++ -std=c++0x -I ../external_libraries/boost/
else
CXX = c++ -stdlib=libc++ -std=c++0x -I ../external_libraries/boost/
endif


all: lambda unicode

clean:
	rm lambda;
	rm unicode;

lambda: lambda.cc
	$(CXX) -o lambda lambda.cc && ./lambda

unicode: unicode.cc
	$(CXX) -o unicode unicode.cc && ./unicode