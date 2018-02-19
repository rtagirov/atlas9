      module nlte_module

      implicit none

      integer                                     :: Nnlte, NAnlte

      character(len=2), allocatable, dimension(:) :: symnlte

      integer, allocatable, dimension(:)          :: nfir, nlas

      real*8,  allocatable, dimension(:)          :: Gnlte, Enlte

      integer, allocatable, dimension(:)          :: Znlte

      logical, dimension(30)                      :: nlte

      integer, allocatable, dimension(:)          :: nis
      integer, allocatable, dimension(:, :)       :: nlev

      real*8,  allocatable, dimension(:, :, :, :) :: dep

      real*8,  allocatable, dimension(:)          :: bHII

      real*8,  allocatable, dimension(:, :, :)    :: bC
      real*8,  allocatable, dimension(:, :, :)    :: bMg
      real*8,  allocatable, dimension(:, :, :)    :: bAl
      real*8,  allocatable, dimension(:, :, :)    :: bSi
      real*8,  allocatable, dimension(:, :, :)    :: bFe

      real*8,  allocatable, dimension(:)          :: acool_ato
      real*8,  allocatable, dimension(:)          :: acool_mol

      real*8,  allocatable, dimension(:)          :: opAlI, sAlI
      real*8,  allocatable, dimension(:)          :: opMgI, sMgI
      real*8,  allocatable, dimension(:)          :: opSiI, sSiI
      real*8,  allocatable, dimension(:)          :: opCI,  sCI
      real*8,  allocatable, dimension(:)          :: opFeI, sFeI

      real*8, allocatable, dimension(:, :)        :: xnfal

      contains

      subroutine read_datom(mode,
     $                      n,
!     $                      LEVEL,
     $                      ncharg,
     $                      weight,
     $                      elevel,
!     $                      EION,
!     $                      MAINQN,
!     $                      EINST,
!     $                      ALPHA,
!     $                      SEXPO,
!     $                      AGAUNT,
!     $                      COCO,
!     $                      KEYCOL,
!     $                      ALTESUM,
!     $                      INDNUP, 
!     $                      INDLOW,
!     $                      LASTIND,
     $                      natom,
!     $                      ELEMENT,
!     $                      symbol,
!     $                      NOM,
!     $                      KODAT,
!     $                      ATMASS,
!     $                      STAGE,
     $                      nfirst,
     $                      nlast)!,
!     $                      WAVARR,
!     $                      SIGARR,
!     $                      eleatnum,
!     $                      levatnum)
!     $                      NFDIM)

      use file_operations
      use utils

      implicit none

      character (len = 4), intent(in)                 :: mode

!      integer, intent(in)                                    :: NFDIM

      integer, intent(out)                            :: n, natom!, LASTIND

!      integer, intent(out), allocatable, dimension(:)        :: KODAT, NOM, NFIRST, NLAST, INDLOW, INDNUP, NCHARG, MAINQN
      integer, intent(out), allocatable, dimension(:) :: nfirst, nlast

      integer, intent(out), allocatable, dimension(:) :: ncharg

!      real*8,  intent(out), allocatable, dimension(:)        :: ATMASS, ALPHA, EION, ELEVEL, SEXPO, WEIGHT, STAGE
      real*8,  intent(out), allocatable, dimension(:) :: elevel, weight

!      real*8,  intent(out), allocatable, dimension(:, :)     :: ALTESUM, EINST, SIGARR, WAVARR

!      real*8,  intent(out), allocatable, dimension(:, :, :)  :: COCO

!      character*2, intent(out), allocatable, dimension(:)    :: symbol

!      character*4, intent(out), allocatable, dimension(:, :) :: keycol

!      character*8, intent(out), allocatable, dimension(:)    :: agaunt

!      character*10, intent(out), allocatable, dimension(:)   :: level, element

!     local variables
      character*10, allocatable, dimension(:) :: level

      integer,      allocatable, dimension(:) :: nom

!     eleatnum - atomic number of an element, levatnum - atomic number of a level
      integer, allocatable, dimension(:)      :: eleatnum, levatnum

!      integer      :: I, IND, IRANGE, J, LEV, LEVSEQ, LOW, LOWP
      integer      :: i, j, lev
!      integer      :: NA, NCHG, NUP, NW
      integer      :: na, nchg, nw
!      real*8       :: ALPLOW, AUPLOW
!      real*8       :: ASUM, CO1, CO2, CO3, CO4, COEFF1, COEFF2
!      real*8       :: E, ELEV, F, GFG, SLOW, SIGMA
      real*8       :: elev
!      character*8  :: AGLOW

      integer      :: elenum, levnum, linnum

      logical      :: nlteelem

      integer      :: un

      real*8       :: dn

      integer      :: di

      character*10 :: ds

!      integer     :: num_i_stages

!      integer, dimension(:), allocatable :: num_stage_lev

!      constants
!      real*8, parameter :: ONE = 1.D+0

!     strings
      CHARACTER (len = 80) :: karte
!      CHARACTER*10 LEVUP, LEVLOW, LINECA, lread
      CHARACTER (len = 10) :: lread
!      CHARACTER*4  CEY
!      CHARACTER*3  KRUDI

      CHARACTER (len = *), parameter ::
     $ FORMAT_LEVEL =   '(12X,A10,1X,I2,1X,I4,2F10.0,1X,I2)',
     $ FORMAT_ELEMENT = '(12X,A10,2X,A2,4X,F6.2,3X,F5.0)'!,
!     $ FORMAT_LTESUM =  '(10X,A10,1X,A8,1X,G9.0,1X,F7.0,1X,F7.0)'

!     ---------------------- ALLOCATION ------------------

      call datom_nums(mode, elenum, levnum)!, linnum)

      print*, 'read_datom: ', mode, elenum, levnum!, linnum

!      allocate(kodat(30))
      allocate(nfirst(elenum))
      allocate(nlast(elenum))
!      allocate(atmass(elenum))
!      allocate(stage(elenum))
      allocate(symnlte(elenum))
!      allocate(element(elenum))
      allocate(eleatnum(elenum))

      allocate(levatnum(levnum))
      allocate(nom(levnum))
      allocate(ncharg(levnum))
!      allocate(mainqn(levnum))
!      allocate(alpha(levnum))
!      allocate(eion(levnum))
      allocate(elevel(levnum))
!      allocate(sexpo(levnum))
      allocate(weight(levnum))
!      allocate(agaunt(levnum))
      allocate(level(levnum))

!      allocate(coco(levnum, levnum, 4))
!      allocate(keycol(levnum, levnum))
!      allocate(einst(levnum, levnum))
!      allocate(altesum(4, levnum))

!      allocate(indlow(linnum))
!      allocate(indnup(linnum))

!      allocate(sigarr(levnum, NFDIM))
!      allocate(wavarr(levnum, NFDIM))

!     ------------------- INITIALIZATION -------------------------

      natom = 0; n = 0
!      levseq = 0

!      KODAT(:) = 0
!      ALTESUM(1, :) = -one
!      COCO(:, :, :) = 0d0
!      KEYCOL(:, :)  = '    '
!      EINST (:, :)  = -one

      nfirst(:) = 0
      nlast(:) =  0

!      indlow(:) = 0
!      indnup(:) = 0

      nom(:) =      0
      eleatnum(:) = 0
      levatnum(:) = 0
      ncharg(:) =   0
!      mainqn(:) =   0

!      atmass(:) =  0.0D0
!      alpha(:) =   0.0D0

!      stage(:) =   0.0D0
!      eion(:) =    0.0D0
      elevel(:) =  0.0D0

!      sexpo(:) =   0.0D0
      weight(:) =  0.0D0

!      agaunt(:) = '        '

!      wavarr(:, :) = 0.0d0
!      sigarr(:, :) = 0.0d0

      un = 1803; open(unit = un, file = nlte_file, action = 'read')

!     ---------------------- EXECUTION -----------------------------

    1 read(un, '(A)', end = 66) karte

      if (karte(:1) == '*' )               goto 1 ! ignore lines startring with '*'

      if     (mode == 'full') then

          if (karte(:10) == 'ELEMENT   ')  goto 5

      elseif (mode == 'nlte') then

          if (karte(:10) == 'ELEMENT   ')  then

              if (index(karte, 'NLTE') /= 0) then

                  nlteelem = .true.

                  goto 5

              else

                  nlteelem = .false.

                  goto 1

              endif

          endif

          if (.not. nlteelem) goto 1

      endif

      if (karte(:10) == 'LEVEL     ') goto 10

!      if (karte(:10) == 'LINE      ') goto 20
      if (karte(:10) == 'LINE      ') goto 1

!      if (karte(:10) == 'CONTINUUM ') goto 30
      if (karte(:10) == 'CONTINUUM ') goto 1

      call error('atlas9.nlte.F: '//
     $           'subroutine read_datom: mode = '//mode//
     $           '; unrecognized data input in '//
     $           nlte_file//': '//karte)

      goto 1

!     ELEMENTS ---------------------------------------------------------
    5 natom = natom + 1

!      if (natom .gt. 30) stop 'datom: natom > 30'

!      LEVSEQ = 0

!      read(karte, format_element) element(natom), symbol(natom), atmass(natom), stage(natom)
      read(karte, format_element) ds, symnlte(natom), dn, dn

      selectcase (symnlte(natom))

         case('H '); symnlte(natom) = 'H'
         case('B '); symnlte(natom) = 'B'
         case('C '); symnlte(natom) = 'C'
         case('N '); symnlte(natom) = 'N'
         case('O '); symnlte(natom) = 'O'
         case('F '); symnlte(natom) = 'F'
         case('P '); symnlte(natom) = 'P'
         case('S '); symnlte(natom) = 'S'
         case('K '); symnlte(natom) = 'K'
         case('V '); symnlte(natom) = 'V'

      endselect

!      IF ((ELEMENT(NATOM) .EQ. 'HELIUM    ') .AND. (SYMBOL(NATOM) .EQ. 'HE' .or. SYMBOL(NATOM) .EQ. 'He')) THEN
!
!             KODAT(1) = NATOM; eleatnum(NATOM) = 2
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'HYDROGEN  ') .AND. (SYMBOL(NATOM) .EQ. 'H ')) THEN
!
!             KODAT(2) = NATOM; eleatnum(NATOM) = 1
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'LITHIU    ') .AND. (SYMBOL(NATOM) .EQ. 'Li')) THEN
!
!             KODAT(3) = NATOM; eleatnum(NATOM) = 3
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'BERRYL    ') .AND. (SYMBOL(NATOM) .EQ. 'Be')) THEN
!
!             KODAT(4) = NATOM; eleatnum(NATOM) = 4
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'BOR       ') .AND. (SYMBOL(NATOM) .EQ. 'B ')) THEN
!
!             KODAT(5) = NATOM; eleatnum(NATOM) = 5
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'CARBON    ') .AND. (SYMBOL(NATOM) .EQ. 'C ')) THEN
!
!             KODAT(6) = NATOM; eleatnum(NATOM) = 6
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'NITROG  ') .AND. (SYMBOL(NATOM) .EQ. 'N ')) THEN
!
!             KODAT(7) = NATOM; eleatnum(NATOM) = 7
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'OXYGEN    ') .AND. (SYMBOL(NATOM) .EQ. 'O ')) THEN
!
!             KODAT(8) = NATOM; eleatnum(NATOM) = 8
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'FLUOR    ') .AND. (SYMBOL(NATOM) .EQ. 'F ')) THEN
!
!             KODAT(9) = NATOM; eleatnum(NATOM) = 9
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'NEON     ') .AND. (SYMBOL(NATOM) .EQ. 'Ne')) THEN
!
!             KODAT(10) = NATOM; eleatnum(NATOM) = 10
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'NATRIUM   ') .AND. (SYMBOL(NATOM) .EQ. 'Na')) THEN
!
!             KODAT(11) = NATOM; eleatnum(NATOM) = 11
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'MAGNES    ') .AND. ((SYMBOL(NATOM) .EQ. 'Mg') .or. (SYMBOL(NATOM) .EQ. 'MG'))) THEN
!
!             KODAT(12) = NATOM; eleatnum(NATOM) = 12
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'ALUMIN    ') .AND. ((SYMBOL(NATOM) .EQ. 'Al').or.(SYMBOL(NATOM) .EQ. 'AL'))) THEN
!
!             KODAT(13) = NATOM; eleatnum(NATOM) = 13
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'SILICON   ') .AND. ((SYMBOL(NATOM) .EQ. 'Si').or.(SYMBOL(NATOM) .EQ. 'SI'))) THEN
!
!             KODAT(14) = NATOM; eleatnum(NATOM) = 14
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'PHOSPH    ') .AND. (SYMBOL(NATOM) .EQ. 'P ')) THEN
!
!             KODAT(15) = NATOM; eleatnum(NATOM) = 15
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'SULPHUR   ') .AND. (SYMBOL(NATOM) .EQ. 'S ')) THEN
!
!             KODAT(16) = NATOM; eleatnum(NATOM) = 16
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'CHLOR     ') .AND. (SYMBOL(NATOM) .EQ. 'Cl')) THEN
!
!             KODAT(17) = NATOM; eleatnum(NATOM) = 17
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'ARGON    ') .AND. (SYMBOL(NATOM) .EQ. 'Ar')) THEN
!
!             KODAT(18) = NATOM; eleatnum(NATOM) = 18
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'POTASS    ') .AND. (SYMBOL(NATOM) .EQ. 'K ')) THEN
!
!             KODAT(19) = NATOM; eleatnum(NATOM) = 19
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'CALCIUM   ') .AND. ((SYMBOL(NATOM) .EQ. 'Ca') .or. (SYMBOL(NATOM) .EQ. 'CA'))) THEN
!
!             KODAT(20) = NATOM; eleatnum(NATOM) = 20
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'SCANDI    ') .AND. (SYMBOL(NATOM) .EQ. 'Sc')) THEN
!
!             KODAT(21) = NATOM; eleatnum(NATOM) = 21
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'TITAN    ') .AND. (SYMBOL(NATOM) .EQ. 'Ti')) THEN
!
!             KODAT(22) = NATOM; eleatnum(NATOM) = 22
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'VANADI    ') .AND. (SYMBOL(NATOM) .EQ. 'V ')) THEN
!
!             KODAT(23) = NATOM; eleatnum(NATOM) = 23
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'CHROM    ') .AND. (SYMBOL(NATOM) .EQ. 'Cr')) THEN
!
!             KODAT(24) = NATOM; eleatnum(NATOM) = 24
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'MANGAN    ') .AND. (SYMBOL(NATOM) .EQ. 'Mn')) THEN
!
!             KODAT(25) = NATOM; eleatnum(NATOM) = 25
!
!CMH  MODEL ATOM OF "IRON" DECODED
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'IRON      ') .AND. ((SYMBOL(NATOM) .EQ. 'Fe') .or. (SYMBOL(NATOM) .EQ. 'FE'))) THEN
!
!             KODAT(26) = NATOM; eleatnum(NATOM) = 26
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'COBALT    ') .AND. (SYMBOL(NATOM) .EQ. 'Co')) THEN
!
!             KODAT(27) = NATOM; eleatnum(NATOM) = 27
!
!CMH  MODEL ATOM OF "NICKEL" DECODED
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'NICKEL    ') .AND. ((SYMBOL(NATOM) .EQ. 'Ni') .or. (SYMBOL(NATOM) .EQ. 'NI'))) THEN
!
!             KODAT(28) = NATOM; eleatnum(NATOM) = 28
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'COPPER    ') .AND. (SYMBOL(NATOM) .EQ. 'Cu')) THEN
!
!             KODAT(29) = NATOM; eleatnum(NATOM) = 29
!
!      ELSEIF ((ELEMENT(NATOM) .EQ. 'ZINC    ') .AND. (SYMBOL(NATOM) .EQ. 'Zn')) THEN
!
!             KODAT(30) = NATOM; eleatnum(NATOM) = 30
!
!      ELSE

!          print*, 'UNKNOWN ELEMENT DECODED'
!          print*, KARTE
!          STOP    'ERROR'

!      ENDIF

      goto 1

!     LEVELS -----------------------------------------------------------
   10 n = n + 1

!      IF (LEVSEQ .NE. 0) THEN

!          print*, 'DATOM: LEVEL CARD OUT OF SEQUENCE'

!          STOP 'ERROR'

!      ENDIF

!      IF (NATOM .NE. 0) THEN

      nom(n) = natom

      levatnum(n) = eleatnum(natom)

!      ENDIF

      nchg = 0
      nw =   0
      elev = 0.0d0
!      e =    0.0d0
!      mqn =  0

      read(karte, format_level) lread, nchg, nw, elev, dn, di

      level(n) = lread
      ncharg(n) = nchg
      elevel(n) = elev
!      MAINQN(N) = mqn
      weight(n) = float(nw)

!      IF (ELEVEL(N).EQ..0 .AND. MAINQN(N).LE.1) EION(N)=E

!      IF (ELEVEL(N).NE..0 .AND. MAINQN(N).LE.1 .AND. (E.NE.0.))
!     $   EION(N)=E-ELEVEL(N) 

      goto 1

   66 close(un)
     
!     some checks of the data
      if (n == 0) call error('atlas9.nlte.F: subroutine read_datom:
     $ no energy levels found. Abort.')

!     all elements are checked one by one for existence of any level
   53 do 54 na = 1, natom

      do 56 i = 1, n

      if (nom(i) == na) goto 54

   56 continue

      call error('atlas9.nlte.F: subroutine read_datom:
     $ element without any level decoded. Abort.')

   54 continue

!     levels are checked for correct element membership
      do 55 j = 1, n

      if (level(j)(:1) /= symnlte(nom(j))(:1))
     $ call error('atlas9.nlte.F: subroutine read_datom:
     $ wrong element membership of levels. Abort.')

   55 continue
     
!     generate vectors nfirst, nlast: first and last level of each element
      do 90 na = 1, natom

          if (na == 1) nfirst(na) = 1

          if (na /= 1) nfirst(na) = nlast(na - 1) + 1

          if (na < natom) then

             do lev = 1, N

                if (nom(lev) == na + 1) then

                    nlast(na) = lev - 1

                    goto 91

                endif

             enddo

             call error('atlas9.nlte.F: subroutine read_datom:
     $ construction of nfirst and nlast did not work. Abort.')

   91 continue

          else

             nlast(na) = n

          endif

   90 continue 

      return

      end subroutine read_datom

      subroutine datom_nums(mode, elenum, levnum)!, linnum)

      use utils
      use file_operations

      character (len = 4), intent(in) :: mode

      integer, intent(out)            :: elenum, levnum!, linnum

      character (len = 100)           :: str

      logical                         :: element, level, line, continuum

      logical                         :: nlteelem

      integer                         :: un, io

      if (mode /= 'nlte' .and. mode /= 'full')
     $ call error('atlas9.nlte.F: subroutine datom_nums:
     $ mode is not recognized. Abort.')

      elenum = 0
      levnum = 0
!      linnum = 0

      un = 9465; open(unit = un, file = nlte_file, action = 'read')

      io = 0

      if     (mode == 'full') then

          do while (io == 0)

    1         read(un, '(A)', iostat = io) str

              if (io /= 0) exit

              if (str(:1) == '*' ) goto 1 ! ignore lines startring with '*'

              element =   str(:10) == 'ELEMENT   '
              level =     str(:10) == 'LEVEL     '
              line =      str(:10) == 'LINE      '
              continuum = str(:10) == 'CONTINUUM '

              if (element)   elenum = elenum + 1
              if (level)     levnum = levnum + 1
!              if (line)      linnum = linnum + 1
              if (line)      goto 1
              if (continuum) goto 1

              if (.not. element .and. 
     $            .not. level   .and.
     $            .not. line    .and.
     $            .not. continuum)
     $            call error('atlas9.nlte.F: subroutine datom_nums: '//
     $                       'mode = '//mode//
     $                       '; unrecognized data input in '//
     $                       nlte_file//':      '//str)

          enddo

      elseif (mode == 'nlte') then

          do while (io == 0)

    2         read(un, '(A)', iostat = io) str

              if (io /= 0) exit

              if (str(:1) == '*' ) goto 2 ! ignore lines startring with '*'

              element =   str(:10) == 'ELEMENT   '
              level =     str(:10) == 'LEVEL     '
              line =      str(:10) == 'LINE      '
              continuum = str(:10) == 'CONTINUUM '

              if (element) then

                  if (index(str, 'NLTE') /= 0) then

                      nlteelem = .true.

                      elenum = elenum + 1

                      goto 2

                  else

                      nlteelem = .false.

                      goto 2

                  endif

              endif

              if (.not. nlteelem) goto 2

              if (level    ) levnum = levnum + 1
!              if (line     ) linnum = linnum + 1
              if (line     ) goto 2
              if (continuum) goto 2

              if (.not. element .and.
     $            .not. level   .and.
     $            .not. line    .and.
     $            .not. continuum)
     $            call error('atlas9.nlte.F: subroutine datom_nums: '//
     $                       'mode = '//mode//
     $                       '; unrecognized data input in '//
     $                       nlte_file//':      '//str)

          enddo

      endif

      close(un)

      end subroutine datom_nums

      subroutine ion_stage_info(na, n, nf, nl, Z)

!      use utils

      implicit none

      integer, intent(in)                :: n, na

!      character (len = 2), intent(in), dimension(natom) :: symbol

      integer, intent(in), dimension(na) :: nf, nl

      integer, intent(in), dimension(n)  :: Z

!      integer,             dimension(na) :: num_i_stages

!      integer, allocatable, dimension(:) :: num_stage_lev

!      integer                                           :: un, i, sc, k, msc
      integer                                           :: i, sc, k, msc

!      un = 1047; open(unit = un, file = 'istageinfo.out', action = 'write')

      allocate(nis(na))

      do k = 1, na

!     number of ionization stsages within each element
          nis(k) = 1

          do i = nf(k), nl(k) - 1

              if (Z(i + 1) /= Z(i)) nis(k) = nis(k) + 1
    
          enddo

      enddo

      msc = maxval(nis)

      allocate(nlev(na, msc))

      do k = 1, na

         nlev(k, :) = 0

         sc = 1

         do i = nf(k), nl(k)

            nlev(k, sc) = nlev(k, sc) + 1

            if (i == nl(k)) exit

            if (Z(i + 1) /= Z(i)) sc = sc + 1

         enddo

!         write(un, '(A2,2x,i2,$)') symbol(k), num_i_stages(k)

!         do i = 1, msc

!            if (i /= msc) write(un, '(2x,i2,$)') num_stage_lev(i)
!            if (i == msc) write(un, '(2x,i2)')   num_stage_lev(i)

!         enddo

      enddo

!      deallocate(num_stage_lev)

!      close(un)

      return

      end subroutine ion_stage_info

      subroutine nlte_elem(nrhox)

      use file_operations

      integer, intent(in)       :: nrhox

      integer                   :: un, io, k

      character (len = 150)     :: l

!      character (len = 2)       :: sym

!      logical                   :: some_elements_are_in_nlte, innlte
      logical                   :: innlte

      nlte(1 : 30) = .false.

      call ion_stage_info(NAnlte, Nnlte, nfir, nlas, Znlte)

      do k = 1, NAnlte

         write(*, '(A,4(2x,I2))') symnlte(k),
     $ nis(k), nlev(k, 1), nlev(k, 2), nlev(k, 3)

      enddo

!      stop

!      un = 1748

!      open(unit = un, file = 'istageinfo.inp', action = 'read')

!      read(un, *) (sym,
!     $             nis(k),
!     $             nl(k, 1),
!     $             nl(k, 2),
!     $             nl(k, 3),
!     $             k = 1, 30)

!      close(un)

      allocate(dep(30, maxval(nis), maxval(nlev), nrhox))

      allocate(bHII(nrhox))

      allocate(bC (maxval(nis), maxval(nlev(6, :)),  nrhox))
      allocate(bMg(maxval(nis), maxval(nlev(12, :)), nrhox))
      allocate(bAl(maxval(nis), maxval(nlev(13, :)), nrhox))
      allocate(bSi(maxval(nis), maxval(nlev(14, :)), nrhox))
      allocate(bFe(maxval(nis), 48,                  nrhox))

      dep =  1.0d0

      bHII = 1.0d0

      bC   = 1.0d0
      bMg  = 1.0d0
      bAl  = 1.0d0
      bSi  = 1.0d0
      bFe  = 1.0d0

!      inquire(file = nlte_file, exist = some_elements_are_in_nlte)

!      if (some_elements_are_in_nlte) then

      un = 1539; open(unit = un, file = nlte_file, action = 'read')

      io = 0

      do while (io == 0)

         read(un, '(A)', iostat = io) l

         if (io /= 0) exit

         if (index(l, 'ELEMENT') /= 0) then

            innlte = index(l, 'NLTE') /= 0

            if (index(l,'HYDROGEN') /= 0.and.innlte) nlte(1) =.true.
            if (index(l,'HELIUM')   /= 0.and.innlte) nlte(2) =.true.
            if (index(l,'LITHIU')   /= 0.and.innlte) nlte(3) =.true.
            if (index(l,'BERRYL')   /= 0.and.innlte) nlte(4) =.true.
            if (index(l,'BOR')      /= 0.and.innlte) nlte(5) =.true.
            if (index(l,'CARBON')   /= 0.and.innlte) nlte(6) =.true.
            if (index(l,'NITROG')   /= 0.and.innlte) nlte(7) =.true.
            if (index(l,'OXYGEN')   /= 0.and.innlte) nlte(8) =.true.
            if (index(l,'FLUOR')    /= 0.and.innlte) nlte(9) =.true.
            if (index(l,'NEON')     /= 0.and.innlte) nlte(10)=.true.
            if (index(l,'NATRIUM')  /= 0.and.innlte) nlte(11)=.true.
            if (index(l,'MAGNES')   /= 0.and.innlte) nlte(12)=.true.
            if (index(l,'ALUMIN')   /= 0.and.innlte) nlte(13)=.true.
            if (index(l,'SILICON')  /= 0.and.innlte) nlte(14)=.true.
            if (index(l,'PHOSPH')   /= 0.and.innlte) nlte(15)=.true.
            if (index(l,'SULPHUR')  /= 0.and.innlte) nlte(16)=.true.
            if (index(l,'CHLOR')    /= 0.and.innlte) nlte(17)=.true.
            if (index(l,'ARGON')    /= 0.and.innlte) nlte(18)=.true.
            if (index(l,'POTASS')   /= 0.and.innlte) nlte(19)=.true.
            if (index(l,'CALCIUM')  /= 0.and.innlte) nlte(20)=.true.
            if (index(l,'SCANDI')   /= 0.and.innlte) nlte(21)=.true.
            if (index(l,'TITAN')    /= 0.and.innlte) nlte(22)=.true.
            if (index(l,'VANADI')   /= 0.and.innlte) nlte(23)=.true.
            if (index(l,'CHROM')    /= 0.and.innlte) nlte(24)=.true.
            if (index(l,'MANGAN')   /= 0.and.innlte) nlte(25)=.true.
            if (index(l,'IRON')     /= 0.and.innlte) nlte(26)=.true.
            if (index(l,'COBALT')   /= 0.and.innlte) nlte(27)=.true.
            if (index(l,'NICKEL')   /= 0.and.innlte) nlte(28)=.true.
            if (index(l,'COPPER')   /= 0.and.innlte) nlte(29)=.true.
            if (index(l,'ZINC')     /= 0.and.innlte) nlte(30)=.true.

         endif

      enddo

      close(un)

      return

      end subroutine nlte_elem

      subroutine nlte_depart(nrhox)

      use utils
      use string_operations ! function str beloew is described here

      include 'common.sizebl'
      include 'common.stateb'
      include 'common.depart'

      integer, intent(in) :: nrhox

      real*8, dimension(nrhox) :: val

      character(:), allocatable :: lev, stage, sym

      integer :: k, i, l

      call read_nlte_lev(nrhox, 'ELECTR', 'pop', val)

      xne(1 : nrhox) = xnatom(1 : nrhox) * val(1 : nrhox)

!------------- READING NLTE DEPARTURES ----------------------

      do k = 1, 30

         if (.not. nlte(k)) cycle

         do i = 1, nis(k)

            selectcase (i)

                case(1); stage = 'I'
                case(2); stage = 'II'
                case(3); stage = 'III'
                case(4); stage = 'IV'
                case(5); stage = 'V'
                case(6); stage = 'VI'
                case(7); stage = 'VII'
                case(8); stage = 'VIII'
                case(9); stage = 'IX'

                case default; call error('atlas9.nlte.F: subroutine '//
     $                                   'nlte_depart: '//
     $                                   'ionization stage '//
     $                                   'not recognized. Abort.')

            endselect

            do l = 1, nlev(k, i)

               sym = trim(adjustl(symnlte(k)))

               lev = sym//stage//str(l)

               if (k == 1.and.i == 1) lev = 'HMINUS'
               if (k == 1.and.i == 2) lev = sym//'I'//str(l)
               if (k == 1.and.i == 3) lev = 'HII'

               call read_nlte_lev(nrhox, lev, 'dep', val)

               dep(k, i, l, 1 : nrhox) = val(1 : nrhox)

            enddo

         enddo

      enddo

!------------- ASSIGNING NLTE DEPARTURES -------------------

!                   HYDROGEN

      bmin(1 : nrhox) = dep(1, 1, 1, :)

      forall (l = 1 : 8) bhyd(1 : nrhox, l) = dep(1, 2, l, :)

      bHII = dep(1, 3, 1, :)

!                   CARBON

      bC(1, 1 : nlev(6, 1), :) = dep(6, 1, 1 : nlev(6, 1), :)
      bC(2, 1 : nlev(6, 2), :) = dep(6, 2, 1 : nlev(6, 2), :)

!                  MAGNESIUM

      bMg(1, 1 : nlev(12, 1), :) = dep(12, 1, 1 : nlev(12, 1), :)
      bMg(2, 1 : nlev(12, 2), :) = dep(12, 2, 1 : nlev(12, 2), :)

!                  ALUMINUM

      bAl(1, 1 : nlev(13, 1), :) = dep(13, 1, 1 : nlev(13, 1), :)
      bAl(2, 1 : nlev(13, 2), :) = dep(13, 2, 1 : nlev(13, 2), :)

!                  SILICON

      bSi(1, 1 : nlev(14, 1), :) = dep(14, 1, 1 : nlev(14, 1), :)
      bSi(2, 1 : nlev(14, 2), :) = dep(14, 2, 1 : nlev(14, 2), :)

!                   IRON

!     first nlev(26, 1) levels of neutral iron (calculated in the NESSY part)
      bFe(1, 1 : nlev(26, 1), :) = dep(26, 1, 1 : nlev(26, 1), :)
      
!     the remaining levels of neutral iron (connection to the last neutral NLTE level)
!      forall(l = nl(26, 1) + 1 : 48) bFe(1, l, :) = bFe(1, nl(26, 1), :)

!     the remaining levels of neutral iron (connection to the first neutral NLTE level)
      forall(l = nlev(26, 1) + 1 : 48) bFe(1, l, :) = bFe(1, 1, :)

      bFe(2, 1 : nlev(26, 2), :) = dep(26, 2, 1 : nlev(26, 2), :)

      end subroutine nlte_depart

      subroutine read_nlte_lev(nrhox, levname, mode, val)

      use utils
      use file_operations

      implicit none

      integer, intent(in) :: nrhox

      character (len = *), intent(in) :: levname

      character (len = 3), intent(in) :: mode

      real*8, dimension(nrhox), intent(out) :: val

      real*8, allocatable, dimension(:) :: val1, val2, val3, val4, val5

      character (len = 10000) :: s

      integer :: un, j

      allocate(val1(nrhox))
      allocate(val2(nrhox))
      allocate(val3(nrhox))
      allocate(val4(nrhox))
      allocate(val5(nrhox))

      un = 1847

      open(unit = un, file = nltepop_dir//levname, action = 'read')

      read(un, *) s

      read(un, *) (val1(j), val2(j), val3(j), val4(j), val5(j),
     $             j = 1, nrhox)

      close(un)

      if (mode == 'pop') val(1 : nrhox) = val4(1 : nrhox)

      if (mode == 'dep') val(1 : nrhox) = val5(1 : nrhox)

      if (mode /= 'pop' .and. mode /= 'dep')
     $ call error('subroutine read_nlte_lev: mode is not recognized.
     $ Abort.')

      deallocate(val1)
      deallocate(val2)
      deallocate(val3)
      deallocate(val4)
      deallocate(val5)

      return

      end subroutine read_nlte_lev

      subroutine print_pop(filename, pop)

      use file_operations

      include 'common.sizebl'
      include 'common.rhoxbl'

      character (len = *),      intent(in) :: filename

      real*8, dimension(nrhox), intent(in) :: pop

      integer                              :: un, j

      logical                              :: exists

      inquire(file = ltepop_dir//filename, exist = exists)

      if (.not. exists) then

          un = 576

          open(unit = un, file = ltepop_dir//filename, action = 'write')

          write(un, '(es15.7)') (pop(j), j = 1, nrhox)

          close(un)

      endif

      return

      end subroutine print_pop

      subroutine read_nist(fn, gt, et, u)

      use file_operations
      use utils

      character (len = *), intent(in)                  :: fn

      character (len = 2), intent(in), optional        :: u

      real*8,   allocatable, dimension(:), intent(out) :: gt, et

      real*8,   allocatable, dimension(:)              :: J, E

      integer,  allocatable, dimension(:)              :: tnum

      character (len = 4), allocatable, dimension(:)   :: tnam

      real*8                                           :: g

      integer                                          :: nl

      integer                                          :: un, i, n

      nl = num_of_lines(fn) - 1

      allocate(tnum(nl))
      allocate(tnam(nl))
      allocate(J(nl))
      allocate(E(nl))

      un = 1742; open(unit = un, file = fn, action = 'read')

      read(un, *)

      read(un, '(I2,4x,A4,4x,F4.2,4x,F9.5)') (tnum(i),
     $                                        tnam(i),
     $                                        J(i),
     $                                        E(i), i = 1, nl)

      close(un)

      allocate(gt(tnum(nl)))
      allocate(et(tnum(nl)))

      do n = 1, tnum(nl)

          gt(n) = 0.0d0
          et(n) = 0.0d0

          do i = 1, nl

             if (tnum(i) /= n) cycle

             g = (2.0d0 * J(i) + 1.0d0)

             gt(n) = gt(n) + g

             et(n) = et(n) + g * E(i)

          enddo

          et(n) = et(n) / gt(n)

      enddo

      deallocate(tnum)
      deallocate(tnam)
      deallocate(J)
      deallocate(E)

      if (present(u)) then

          if (u == 'eV') et = et / 8065.5d0

          if (u /= 'eV') call error('atlas9.nlte.F: '        //
     $                              'subroutine read_nist: ' //
     $                              'energy units are not '  //
     $                              'recognized. Abort.')

      endif

      return

      end subroutine read_nist

      subroutine find_lev_nums(sym, istage, levs)

      use utils

      character (len = *),                intent(in)  :: sym
      character (len = *),                intent(in)  :: istage

      integer, allocatable, dimension(:), intent(out) :: levs

      integer                                         :: na, isn, Z

      integer                                         :: i, l

      selectcase (sym)

         case('H') ; na = 1
         case('He'); na = 2
         case('Li'); na = 3
         case('Be'); na = 4
         case('B') ; na = 5
         case('C') ; na = 6
         case('N') ; na = 7
         case('O') ; na = 8
         case('F') ; na = 9
         case('Ne'); na = 10
         case('Na'); na = 11
         case('Mg'); na = 12
         case('Al'); na = 13
         case('Si'); na = 14
         case('P') ; na = 15
         case('S') ; na = 16
         case('Cl'); na = 17
         case('Ar'); na = 18
         case('K') ; na = 19
         case('Ca'); na = 20
         case('Sc'); na = 21
         case('Ti'); na = 22
         case('V') ; na = 23
         case('Cr'); na = 24
         case('Mn'); na = 25
         case('Fe'); na = 26
         case('Co'); na = 27
         case('Ni'); na = 28
         case('Cu'); na = 29
         case('Zn'); na = 30

         case default; call error('atlas9.nlte.F: subroutine '//
     $                            'find_lev_nums: '//
     $                            'element symbol '//
     $                            'not recognized. Abort.')

      endselect

      selectcase (istage)

         case('I'   ); isn = 1; Z = 0
         case('II'  ); isn = 2; Z = 1
         case('III' ); isn = 3; Z = 2
         case('IV'  ); isn = 4; Z = 3
         case('V'   ); isn = 5; Z = 4
         case('VI'  ); isn = 6; Z = 5
         case('VII' ); isn = 7; Z = 6
         case('VIII'); isn = 8; Z = 7
         case('IX'  ); isn = 9; Z = 8

      case default; call error('atlas9.nlte.F: subroutine '//
     $                          'find_lev_nums: '//
     $                          'ionization stage '//
     $                          'not recognized. Abort.')

      endselect

      allocate(levs(nlev(na, isn)))

      i = 1

      do l = nfir(na), nlas(na)

         if (Znlte(l) /= Z) cycle

         levs(i) = l

         i = i + 1

      enddo

      return

      end subroutine find_lev_nums

      end module
