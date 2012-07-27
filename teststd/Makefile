
UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
CXX = xcrun c++ -std=c++0x -stdlib=libc++ -I ../external_libraries/boost/
EXE = ""
else
CXX = c++ -std=c++0x -I ../external_libraries/boost/
EXE = ".exe"
endif


all: lambda unicode array thread

clean:
	rm lambda$(EXE);
	rm unicode$(EXE);
	rm array$(EXE);
	rm thread$(EXE);

lambda: lambda.cc
	$(CXX) -o lambda$(EXE) lambda.cc && ./lambda

unicode: unicode.cc
	$(CXX) -o unicode$(EXE) unicode.cc && ./unicode

array: array.cc
	$(CXX) -o array$(EXE) array.cc && ./array

thread: thread.cc
	$(CXX) -o thread$(EXE) thread.cc && ./thread

# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
# thread: thread.cc
# 	$(CXX) -o thread$(EXE) thread.cc && ./thread
#
