cmake_minimum_required(VERSION 3.10)
project(shardc)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_VERSION}")
message(STATUS "LLVM includes: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM libraries: ${LLVM_LIBRARY_DIRS}")

file(GLOB_RECURSE SOURCES "src/*.cpp")

add_compile_options(-w)

add_executable(${PROJECT_NAME} ${SOURCES})

target_include_directories(${PROJECT_NAME} PRIVATE ${LLVM_INCLUDE_DIRS})
target_compile_definitions(${PROJECT_NAME} PRIVATE ${LLVM_DEFINITIONS})

if (TARGET LLVM)
    target_link_libraries(${PROJECT_NAME} PRIVATE LLVM)
else()
    llvm_map_components_to_libnames(LLVM_LIBS all)
    target_link_libraries(${PROJECT_NAME} PRIVATE ${LLVM_LIBS})
endif()