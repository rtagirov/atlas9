SRC=atlas9.main.F\
    atlas9.read.F\
    atlas9.kapp.F\
    atlas9.pops.F\
    atlas9.josh.F\
    atlas9.conv.F

OBJDIR=../obj/

OBJ=$(addprefix ${OBJDIR},${SRC:.F=.o})

EXE=${OBJDIR}atlas9.exe

#fortran compiler
FC=ifort

#netcdf library
NETCDFLIB_INC_PATH=-I ../lib/netcdf-3.6.1/include
NETCDFLIB=-L ../obj -lnet -L ../lib/netcdf-3.6.1/lib -lnetcdf

#ifort preprocessor flags
FPPFLAGS=#-DODF

#target definition
all: exe

exe: ${OBJ}
	${FC} ${FPPFLAGS} -o ${EXE} ${OBJ} ${NETCDFLIB_INC_PATH} ${NETCDFLIB}

${OBJDIR}%.o: %.F
	${FC} -c ${FPPFLAGS} $< -o $@

clean:
	rm -vf ${OBJ} ${EXE}
