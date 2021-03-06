
cmake_minimum_required ( VERSION 2.8.8 )
set ( CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/../cmake/modules )

include(${CMAKE_SOURCE_DIR}/../cmake/scripts/components.cmake)
resolve_components(COMPONENTS)

#set(COMPONENTS_RESOLVED True 
#  CACHE BOOL "The components has been resolved")
execute_process(
  COMMAND cmake ".." "-DCOMPONENTS_RESOLVED=1"
  )