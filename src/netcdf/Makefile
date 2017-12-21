SRC = CreateNetCDF.F90\
      OpenNetCDF.F90\
      ReadNetCDF.F90\
      CloseNetCDF.F90\
	  WriteNetCDF.F90\
      handle_error.F90

OBJDIR=../../obj/

OBJ = $(addprefix ${OBJDIR},$(SRC:.F90=.o))

LIB = $(OBJDIR)libnet.a

all: lib

lib: $(OBJ)
	ar crv $(LIB) $(OBJ)

${OBJDIR}%.o: %.F90
	$(FC) -c $(FPPFLAGS) $< $(NETCDFLIB_INC_PATH2) $(STATIC) -o $@

#CreateNetCDF.o: CreateNetCDF.F90
#	$(FC) $(FFLAGS) $(FPPFLAGS) CreateNetCDF.F90 $(NETCDFLIB_INC_PATH2) $(STATIC)

#OpenNetCDF.o: OpenNetCDF.F90
#	$(FC) $(FFLAGS) $(FPPFLAGS) OpenNetCDF.F90 $(NETCDFLIB_INC_PATH2) $(STATIC)

#ReadNetCDF.o: ReadNetCDF.F90
#	$(FC) $(FFLAGS) $(FPPFLAGS) ReadNetCDF.F90 $(NETCDFLIB_INC_PATH2) $(STATIC)

#CloseNetCDF.o: CloseNetCDF.F90
#	$(FC) $(FFLAGS) $(FPPFLAGS) CloseNetCDF.F90 $(NETCDFLIB_INC_PATH2) $(STATIC)

#WriteNetCDF.o: WriteNetCDF.F90
#	$(FC) $(FFLAGS) $(FPPFLAGS) WriteNetCDF.F90 $(NETCDFLIB_INC_PATH2) $(STATIC)

#handle_error.o: handle_error.F90
#	$(FC) $(FFLAGS) $(FPPFLAGS) handle_error.F90 $(NETCDFLIB_INC_PATH2) $(STATIC)

clean:
	rm -vf $(OBJ) $(LIB)
