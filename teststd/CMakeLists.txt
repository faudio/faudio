
cmake_minimum_required(VERSION 2.8)
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/../cmake/modules)

if(APPLE)
  set(CMAKE_CXX_COMPILER 
    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/c++")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
else()
  set(CMAKE_CXX_COMPILER 
    "c++"
    )
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")

include_directories(../external_libraries/boost)
include_directories(../external_libraries/sprout)

# if(APPLE)
  # add_definitions("-stdlib=libc++")
# endif()

find_package(Pthreads)
find_package(Iconv)



## Boost

# Boost headers
include_directories("../external_libraries/boost")

# Boost.System library
aux_source_directory("../external_libraries/boost/libs/system/src" BOOST_SYSTEM_SRC)
add_library (BoostSystem STATIC ${BOOST_SYSTEM_SRC})

# Boost.Filesystem library
aux_source_directory("../external_libraries/boost/libs/filesystem/src" BOOST_FILESYSTEM_SRC)
add_library(BoostFilesystem STATIC ${BOOST_FILESYSTEM_SRC})

# Boost.Thread library
set(BOOST_THREAD_SRC "../external_libraries/boost/libs/thread/src/future.cpp")

if(WIN32)
	aux_source_directory("../external_libraries/boost/libs/thread/src/win32/" BOOST_THREAD_PLATFORM)
	add_definitions(-DBOOST_THREAD_BUILD_LIB)
elseif(PTHREADS_FOUND)
	aux_source_directory("../external_libraries/boost/libs/thread/src/pthread/" BOOST_THREAD_PLATFORM)
	include_directories(${PTHREADS_INCLUDE_DIRS})
else()
	message(FATAL_ERROR "Boost.Thread: Needs either pthreads or Windows")
endif()

add_library(BoostThread STATIC ${BOOST_THREAD_SRC} ${BOOST_THREAD_PLATFORM})

if(WIN32)
  add_definitions(-DBOOST_THREAD_USE_LIB=1)
endif()
add_definitions(-DBOOST_CHRONO_HEADER_ONLY=1)


## Files

add_executable(array              array.cc)
add_executable(bind               bind.cc)
add_executable(constexpr          constexpr.cc)
add_executable(extern_template    extern_template.cc)
add_executable(initializer_list   initializer_list.cc)
add_executable(iterator           iterator.cc)
add_executable(lambda             lambda.cc)
add_executable(mpl                mpl.cc)
add_executable(smart_pointers     smart_pointers.cc)
add_executable(thread             thread.cc)
add_executable(type_traits        type_traits.cc)
add_executable(unicode            unicode.cc)

add_executable(logging            logging.cc)
add_executable(atomic             atomic.cc)


target_link_libraries(thread  BoostThread BoostSystem)
target_link_libraries(unicode ${ICONV_LIBRARIES})


