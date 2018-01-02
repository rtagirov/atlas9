src=CreateNetCDF.F90\
    OpenNetCDF.F90\
    ReadNetCDF.F90\
    CloseNetCDF.F90\
	WriteNetCDF.F90\
    handle_error.F90

objdir=../../obj/

obj=$(addprefix ${objdir},${src:.F90=.o})

lib=${objdir}libnet.a

#fortran compiler
comp=ifort

#netcdf library
netcdflib_inc_path=-I ../../lib/netcdf-3.6.1/include

all: lib

lib: ${obj}
	ar crv ${lib} ${obj}

${objdir}%.o: %.F90
	${comp} -c $< ${netcdflib_inc_path} ${static} -o $@

clean:
	rm -vf ${obj} ${lib}
