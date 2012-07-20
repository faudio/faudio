
# Boost headers
include_directories(boost)

# Boost.System library
aux_source_directory(boost/libs/system/src BOOST_SYSTEM_SRC)
add_library (BoostSystem STATIC ${BOOST_SYSTEM_SRC})

# Boost.Filesystem library
aux_source_directory(boost/libs/filesystem/src BOOST_FILESYSTEM_SRC)
add_library(BoostFilesystem STATIC ${BOOST_FILESYSTEM_SRC})

# Boost.Thread library
set(BOOST_THREAD_SRC boost/libs/thread/src/future.cpp)

if(WIN32)
	aux_source_directory(boost/libs/thread/src/win32/ BOOST_THREAD_PLATFORM)
	add_definitions(-DBOOST_THREAD_BUILD_LIB)
elseif(PTHREADS_FOUND)
	aux_source_directory(boost/libs/thread/src/pthread/ BOOST_THREAD_PLATFORM)
	include_directories(${PTHREADS_INCLUDE_DIRS})
else()
	message(FATAL_ERROR "Boost.Thread: Needs either pthreads or Windows")
endif()

add_library(BoostThread STATIC ${BOOST_THREAD_SRC} ${BOOST_THREAD_PLATFORM})

