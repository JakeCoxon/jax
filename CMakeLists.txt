cmake_minimum_required (VERSION 3.14)

project(jax)

add_compile_options(
  -Wall -Wextra -pedantic -Werror
  -Wno-unused -Wno-unused-function -Wno-unused-label -Wno-unused-parameter -Wno-unused-value -Wno-unused-variable
)

add_executable (${PROJECT_NAME} src/main.cpp)

target_include_directories(${PROJECT_NAME} 
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src
	PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)