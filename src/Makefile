src=atlas9.file.F\
    atlas9.main.F\
    atlas9.read.F\
    atlas9.kapp.F\
    atlas9.pops.F\
    atlas9.josh.F\
    atlas9.conv.F

objdir=../obj/

obj=$(addprefix ${objdir},${src:.F=.o})

exe=${objdir}atlas9.exe

#fortran compiler
comp=ifort

#netcdf library
inc=-I ../lib/netcdf-3.6.1/include
lib=-L ${objdir} -lnet -L ../lib/netcdf-3.6.1/lib -lnetcdf

#regular flags
rf=-module ${objdir}

#preprocessor flags
ppf=#-DODF

#target definition
all: exe

exe: ${obj}
	${comp} ${rf} ${ppf} -o ${exe} ${obj} ${inc} ${lib}

${objdir}%.o: %.F
	${comp} -c ${ppf} $< ${rf} -o $@

clean:
	rm -vf ${obj} ${objdir}*.mod ${exe}
