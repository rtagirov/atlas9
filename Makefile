subdirs=./src/netcdf ./src

all: atlas9

atlas9:
	 @for i in ${subdirs}; do cd $$i; ${MAKE} all; cd ../../; done

clean:
	@for i in ${subdirs}; do cd $$i; ${MAKE} clean; cd ../../; done
