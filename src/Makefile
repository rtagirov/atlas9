SRC = atlas9.main.F\
      atlas9.read.F\
      atlas9.kapp.F\
      atlas9.pops.F\
      atlas9.josh.F\
      atlas9.conv.F

OBJDIR=../obj/

OBJ = $(addprefix ${OBJDIR},$(SRC:.F=.o))

EXE = $(OBJDIR)atlas9.exe

#ifort preprocessor flags
FPPFLAGS = "-DODF"

#target definition
all: exe

exe: $(OBJ)
	$(FC) $(FPPFLAGS) -o $(EXE) $(OBJ) $(NETCDFLIB_INC_PATH1) $(NETCDFLIB)

${OBJDIR}%.o: %.F
	$(FC) -c $(FPPFLAGS) $< -o $@

clean:
	rm -vf $(OBJ) $(EXE)
