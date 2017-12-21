#COMMONS = common.abross \
          common.abtotb \
          common.constb \
          common.contbl \
          common.convbl \
          common.depart \
          common.edenbl \
          common.elembl \
          common.fluxbl \
          common.freqbl \
          common.freset \
          common.height \
          common.ifblkk \
          common.ifequa \
          common.ifopbl \
          common.ionsbl \
          common.iterbl \
          common.junkbl \
          common.meudbl \
          common.musblk \
          common.opsblk \
          common.optotb \
          common.parblk \
          common.ptotal \
          common.putblk \
          common.pzerob \
          common.radblk \
          common.rhoxbl \
          common.sizebl \
          common.stateb \
          common.steplg \
          common.freebl \
          common.tabptv \
          common.taushj \
          common.tcorrb \
          common.odfnlte \
          common.teffbl \
          common.tempbl \
          common.turbpr \
          common.waveyb \
          common.xabund \
          common.xnfblk \
          common.xnfpbl \
          common.xnmolb

SRC = atlas9.main.F \
      atlas9.read0.F \
      atlas9.kapp.F \
      atlas9.pops.F \
      atlas9.josh.F \
      atlas9.conv.F

OBJDIR=../obj/

OBJ = $(addprefix ${OBJDIR},$(SRC:.F=.o))

EXE = $(OBJDIR)atlas9.exe

#AOBJECTS_B = atlas9.main.o atlas9.read0.o atlas9.kapp.o \
#             atlas9.pops.o atlas9b.josh.o atlas9.conv.o

#LFLAGS =

#ifort preprocessor flags
FPPFLAGS = "-DODF"

#FFLAGS = -c

# target definition
all: exe
# -------------------------- ATLAS ------------------------------------

exe: $(OBJ)
#	$(FC) $(LFLAGS) $(FPPFLAGS) -o atlas9.exe $(AOBJECTS) $(INCLUDE) $(NETCDFLIB)
	$(FC) $(FPPFLAGS) -o $(EXE) $(OBJ) $(NETCDFLIB_INC_PATH1) $(NETCDFLIB)
#	mv atlas9.exe *.o -t ../obj/atlas9/
# -------------------------- ATLAS9B -----------------------------------

#atlasb: $(AOBJECTS_B)
#	$(FC) $(LFLAGS) $(FPPFLAGS) -o atlas9b.exe $(AOBJECTS_B)
#	$(FC) $(FPPFLAGS) -o atlas9b.exe $(AOBJECTS_B)

# ----------------------- ATLAS9 DEPENDENCIES ---------------------------

${OBJDIR}%.o: %.F
	$(FC) -c $(FPPFLAGS) $< -o $@

#atlas9.main.o: $(COMMONS) atlas9.main.F
#	$(FC) $(FFLAGS) $(FPPFLAGS) atlas9.main.F

#atlas9.read0.o: $(COMMONS) atlas9.read0.F
#	$(FC) $(FFLAGS) $(FPPFLAGS) atlas9.read0.F

#atlas9.kapp.o: $(COMMONS) atlas9.kapp.F
#	$(FC) $(FFLAGS) $(FPPFLAGS) atlas9.kapp.F

#atlas9.pops.o: $(COMMONS) atlas9.pops.F
#	$(FC) $(FFLAGS) $(FPPFLAGS) atlas9.pops.F

#atlas9.josh.o: $(COMMONS) atlas9.josh.F
#	$(FC) $(FFLAGS) $(FPPFLAGS) atlas9.josh.F

#atlas9.conv.o: $(COMMONS) atlas9.conv.F
#	$(FC) $(FFLAGS) $(FPPFLAGS) atlas9.conv.F

# ----------------------- ATLAS9B DEPENDENCIES ---------------------------

#atlas9b.josh.o: $(COMMONS) atlas9b.josh.f
#	$(FC)  $(FFLAGS) atlas9b.josh.f

# ========================================================================
clean:
	rm -vf $(OBJ) $(EXE)
