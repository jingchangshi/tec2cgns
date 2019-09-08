FC=ifort

DEBUG_FLAGS=-g -O0
CGNS_DIR=../cgns
INC_FLAGS=-I${CGNS_DIR}/include
# CGNS_LIB=${CGNS_DIR}/lib/libcgns.a
LIB_FLAGS=-lcgns -L${CGNS_DIR}/lib

tec2cgns: tec2cgns.f90 udef_ktypes.o
	${FC} ${DEBUG_FLAGS} -o tec2cgns tec2cgns.f90 udef_ktypes.o ${INC_FLAGS} ${LIB_FLAGS}
udef_ktypes.o: udef_ktypes.f90
	${FC} -c udef_ktypes.f90

clean:
	rm *.mod *.o tec2cgns *.cgns

# To run tec2cgns, set up the lib path of libcgns.so
# export LD_LIBRARY_PATH=$(pwd)/../cgns/lib:$LD_LIBRARY_PATH
