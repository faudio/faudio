

if(TRUE)
	include_directories(boost)

	# boost system
	aux_source_directory(boost/libs/system/src/ boost_system )
	add_library(boost_system STATIC ${boost_system})

	# boost filesystem
	aux_source_directory(boost/libs/filesystem/src boost_filesystem_src)
	add_library(boost_filesystem STATIC ${boost_filesystem_src})

	# boost thread
	#aux_source_directory(boost/libs/thread/src/ boost_thread_src )
	list ( APPEND boost_thread_src boost/libs/thread/src/future.cpp )
	
	if(WIN32)
		aux_source_directory(boost/libs/thread/src/win32/ boost_thread_platform )
		add_definitions(-DBOOST_THREAD_BUILD_LIB)
	elseif(PTHREADS_FOUND)
		aux_source_directory(boost/libs/thread/src/pthread/ boost_thread_platform )
	else()
		message(SEND_ERROR "we need to implement boost_thread")
	endif()

	include_directories(${PTHREADS_INCLUDE_DIR})

  # if(LTO)
  #   set_property(TARGET boost_program_options boost_system boost_filesystem
  #          APPEND PROPERTY COMPILE_FLAGS "-flto -flto-report")
  # 
  #   set_property(TARGET boost_program_options boost_system boost_filesystem
  #          APPEND PROPERTY LINK_FLAGS "-flto -flto-report")
  # endif()

  # if (${DSO_VISIBILITY})
  #   remove_definitions(-fvisibility=hidden)
  # endif()

	add_library(boost_thread STATIC ${boost_thread_src} ${boost_thread_platform})


  # if(LTO)
  #   set_property(TARGET boost_thread
  #         APPEND PROPERTY COMPILE_FLAGS "-flto -flto-report")
  # 
  #   set_property(TARGET boost_thread
  #         APPEND PROPERTY LINK_FLAGS "-flto -flto-report")
  # endif()
endif()