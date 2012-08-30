
execute_process(
  COMMAND git submodule update "--init" "external_libraries/boost"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})

# # Boost headers
# include_directories(boost)

# # Boost.System library
# aux_source_directory(boost/libs/system/src BOOST_SYSTEM_SRC)
# add_library (BoostSystem STATIC ${BOOST_SYSTEM_SRC})

# # Boost.Filesystem library
# aux_source_directory(boost/libs/filesystem/src BOOST_FILESYSTEM_SRC)
# add_library(BoostFilesystem STATIC ${BOOST_FILESYSTEM_SRC})

# # Boost.Thread library
# set(BOOST_THREAD_SRC boost/libs/thread/src/future.cpp)

# if(WIN32)
	# aux_source_directory(boost/libs/thread/src/win32/ BOOST_THREAD_PLATFORM)
	# add_definitions(-DBOOST_THREAD_BUILD_LIB)
# elseif(PTHREADS_FOUND)
	# aux_source_directory(boost/libs/thread/src/pthread/ BOOST_THREAD_PLATFORM)
	# include_directories(${PTHREADS_INCLUDE_DIRS})
# else()
	# message(FATAL_ERROR "Boost.Thread: Needs either pthreads or Windows")
# endif()

# add_library(BoostThread STATIC ${BOOST_THREAD_SRC} ${BOOST_THREAD_PLATFORM})

## Boost

# Boost headers
include_directories("boost")

# Boost.System library
aux_source_directory("boost/libs/system/src" BOOST_SYSTEM_SRC)
add_library (BoostSystem STATIC ${BOOST_SYSTEM_SRC})

# Boost.Filesystem library
aux_source_directory("boost/libs/filesystem/src" BOOST_FILESYSTEM_SRC)
add_library(BoostFilesystem STATIC ${BOOST_FILESYSTEM_SRC})

# Boost.Program Options library
# aux_source_directory("boost/libs/program_options/src" BOOST_PROGRAM_OPTIONS_SRC)
# add_library(BoostProgramOptions STATIC ${BOOST_PROGRAM_OPTIONS_SRC})


macro(add_boost_thread)
  # Boost.Thread library
  set(BOOST_THREAD_SRC "boost/libs/thread/src/future.cpp")

  if(WIN32)
  	aux_source_directory("boost/libs/thread/src/win32/" BOOST_THREAD_PLATFORM)
  	add_definitions(-DBOOST_THREAD_BUILD_LIB)
  elseif(PTHREADS_FOUND)
  	aux_source_directory("boost/libs/thread/src/pthread/" BOOST_THREAD_PLATFORM)
  	include_directories(${PTHREADS_INCLUDE_DIRS})
  else()
  	message(FATAL_ERROR "Boost.Thread: Needs either pthreads or Windows")
  endif()

  add_library(BoostThread STATIC ${BOOST_THREAD_SRC} ${BOOST_THREAD_PLATFORM})
  set(BOOST_THREAD_LIB BoostThread)

  add_definitions(-DBOOST_CHRONO_HEADER_ONLY=1)
  add_definitions(-DBOOST_THREAD_USE_LIB=1)
endmacro()

if (USE_BOOST_THREAD)
  add_boost_thread()
endif()

