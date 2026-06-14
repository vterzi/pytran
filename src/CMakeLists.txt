file(GLOB_RECURSE sources "*.f90" "*.F90")
add_library(pytran SHARED ${sources})
