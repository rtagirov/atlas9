FC=ifort

LFLAGS =

#ifort preprocessor flags
FPPFLAGS =#"-DODF"

FFLAGS = -c

#NETCDF library routines

#INCLUDE="-I/mnt/SSD/sim/atlas9/lib/netcdf-3.6.1/include"
#NETCDFLIB="-L/mnt/SSD/sim/atlas9/obj/netcdf -lnet -L/mnt/SSD/sim/atlas9/lib/netcdf-3.6.1/lib -lnetcdf"
NETCDFLIB_INC_PATH1="-I ../lib/netcdf-3.6.1/include"
NETCDFLIB_INC_PATH2="-I ../../lib/netcdf-3.6.1/include"

NETCDFLIB="-L ../obj -lnet -L ../lib/netcdf-3.6.1/lib -lnetcdf"

####################################################################

#SUBDIRS = /mnt/SSD/sim/atlas9/src/netcdf /mnt/SSD/sim/atlas9/src
SUBDIRS = ./src/netcdf ./src

all: atlas9

atlas9:
	 @for i in $(SUBDIRS); do \
                cd $$i; \
                $(MAKE)         \
                        FC=$(FC) \
                        FFLAGS=$(FFLAGS) \
                        FPPFLAGS=$(FPPFLAGS) \
                        FFTLIB=$(FFTLIB) \
                        NETCDFLIB_INC_PATH1=$(NETCDFLIB_INC_PATH1) \
                        NETCDFLIB_INC_PATH2=$(NETCDFLIB_INC_PATH2) \
                        STATIC=$(STATIC) \
                        all ; cd ../../ ;\
          done

clean:
	@for i in $(SUBDIRS); do \
                (cd $$i; \
                $(MAKE) clean ); \
         done
