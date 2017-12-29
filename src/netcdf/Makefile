SRC=CreateNetCDF.F90\
    OpenNetCDF.F90\
    ReadNetCDF.F90\
    CloseNetCDF.F90\
	WriteNetCDF.F90\
    handle_error.F90

OBJDIR=../../obj/

OBJ=$(addprefix ${OBJDIR},${SRC:.F90=.o})

LIB=${OBJDIR}libnet.a

#fortran compiler
FC=ifort

#netcdf library
NETCDFLIB_INC_PATH=-I ../../lib/netcdf-3.6.1/include

all: lib

lib: ${OBJ}
	ar crv ${LIB} ${OBJ}

${OBJDIR}%.o: %.F90
	${FC} -c $< ${NETCDFLIB_INC_PATH} ${STATIC} -o $@

clean:
	rm -vf ${OBJ} ${LIB}
