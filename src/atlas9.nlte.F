      module nlte_module

      implicit none

      logical, dimension(30) :: nlte

      real*8, allocatable, dimension(:) ::    bHII

      real*8, allocatable, dimension(:, :) :: bCI
      real*8, allocatable, dimension(:) ::    bCII

      real*8, allocatable, dimension(:, :) :: bMgI
      real*8, allocatable, dimension(:) ::    bMgII

      real*8, allocatable, dimension(:) ::    bAlI
      real*8, allocatable, dimension(:) ::    bAlII

      real*8, allocatable, dimension(:, :) :: bSiI
      real*8, allocatable, dimension(:) ::    bSiII

      real*8, allocatable, dimension(:, :) :: bFeI
      real*8, allocatable, dimension(:) ::    bFeII

      contains

      subroutine nlte_elem(nrhox)

      use file_operations

      integer, intent(in) :: nrhox

      integer :: un, io

      character (len = 150) :: line

      logical :: nlte_on

      un = 1532

      nlte(1 : 30) = .false.

      allocate(bHII (   nrhox))

      allocate(bCI  (3, nrhox))
      allocate(bCII (   nrhox))

      allocate(bMgI (3, nrhox))
      allocate(bMgII(   nrhox))

      allocate(bAlI (   nrhox))
      allocate(bAlII(   nrhox))

      allocate(bSiI (6, nrhox))
      allocate(bSiII(   nrhox))

      allocate(bFeI (5, nrhox))
      allocate(bFeII(   nrhox))

      bHII  = 1.0d0

      bCI   = 1.0d0
      bCII  = 1.0d0

      bMgI  = 1.0d0
      bMgII = 1.0d0

      bAlI  = 1.0d0
      bAlII = 1.0d0

      bSiI  = 1.0d0
      bSiII = 1.0d0

      bFeI  = 1.0d0
      bFeII = 1.0d0

      inquire(file = nlte_file, exist = nlte_on)

      if (nlte_on) then

          open(unit = un, file = nlte_file, action = 'read')

          io = 0

          do while (io .eq. 0)

             read(un, '(A)', iostat = io) line

             if (io .ne. 0) exit

             if (index(line, 'ELEMENT') .ne. 0) then

                if (index(line, 'HYDROGEN') .ne. 0) nlte(1) =  .true.
                if (index(line, 'HELIUM')   .ne. 0) nlte(2) =  .true.
                if (index(line, 'LITHIU')   .ne. 0) nlte(3) =  .true.
                if (index(line, 'BERRYL')   .ne. 0) nlte(4) =  .true.
                if (index(line, 'BOR')      .ne. 0) nlte(5) =  .true.
                if (index(line, 'CARBON')   .ne. 0) nlte(6) =  .true.
                if (index(line, 'NITROG')   .ne. 0) nlte(7) =  .true.
                if (index(line, 'OXYGEN')   .ne. 0) nlte(8) =  .true.
                if (index(line, 'FLUOR')    .ne. 0) nlte(9) =  .true.
                if (index(line, 'NEON')     .ne. 0) nlte(10) = .true.
                if (index(line, 'NATRIUM')  .ne. 0) nlte(11) = .true.
                if (index(line, 'MAGNES')   .ne. 0) nlte(12) = .true.
                if (index(line, 'ALUMIN')   .ne. 0) nlte(13) = .true.
                if (index(line, 'SILICON')  .ne. 0) nlte(14) = .true.
                if (index(line, 'PHOSPH')   .ne. 0) nlte(15) = .true.
                if (index(line, 'SULPHUR')  .ne. 0) nlte(16) = .true.
                if (index(line, 'CHLOR')    .ne. 0) nlte(17) = .true.
                if (index(line, 'ARGON')    .ne. 0) nlte(18) = .true.
                if (index(line, 'POTASS')   .ne. 0) nlte(19) = .true.
                if (index(line, 'CALCIUM')  .ne. 0) nlte(20) = .true.
                if (index(line, 'SCANDI')   .ne. 0) nlte(21) = .true.
                if (index(line, 'TITAN')    .ne. 0) nlte(22) = .true.
                if (index(line, 'VANADI')   .ne. 0) nlte(23) = .true.
                if (index(line, 'CHROM')    .ne. 0) nlte(24) = .true.
                if (index(line, 'MANGAN')   .ne. 0) nlte(25) = .true.
                if (index(line, 'IRON')     .ne. 0) nlte(26) = .true.
                if (index(line, 'COBALT')   .ne. 0) nlte(27) = .true.
                if (index(line, 'NICKEL')   .ne. 0) nlte(28) = .true.
                if (index(line, 'COPPER')   .ne. 0) nlte(29) = .true.
                if (index(line, 'ZINC')     .ne. 0) nlte(30) = .true.

             endif

          enddo

          close(un)

      endif

      return

      end

      subroutine nlte_depart(nrhox)

      include 'common.stateb'
      include 'common.depart'

      integer, intent(in) :: nrhox

      real*8, dimension(nrhox) :: val

      integer :: n

      call nlte_lev(nrhox, 'ELECTR', 'pop', val)

      xne(1 : nrhox) = xnatom(1 : nrhox) * val(1 : nrhox)

!---------------- HYDROGEN NLTE DEPARTURES ----------------------

      if (nlte(1)) then

         call nlte_lev(nrhox, 'HMINUS', 'dep', bmin)

         do n = 1, 8

            call nlte_lev(nrhox, 'HI'//str(n), 'dep', val)

            bhyd(1 : nrhox, n) = val(1 : nrhox)

         enddo

         call nlte_lev(nrhox, 'HII', 'dep', bHII)

      endif

!---------------- CARBON NLTE DEPARTURES -------------------------

      if (nlte(6)) then

         do n = 1, 3

            call nlte_lev(nrhox, 'CI'//str(n), 'dep', val)

            bCI(n, 1 : nrhox) = val(1 : nrhox)

         enddo

         call nlte_lev(nrhox, 'CII1', 'dep', bCII)

      endif

!---------------- MAGNESIUM NLTE DEPARTURES -------------------------

      if (nlte(12)) then

         do n = 1, 3

            call nlte_lev(nrhox, 'MgI'//str(n), 'dep', val)

            bMgI(n, 1 : nrhox) = val(1 : nrhox)

         enddo

         call nlte_lev(nrhox, 'MgII1', 'dep', bMgII)

      endif

!---------------- ALUMINUM NLTE DEPARTURES -------------------------

      if (nlte(13)) then

         call nlte_lev(nrhox, 'AlI1',  'dep', bAlI)

         call nlte_lev(nrhox, 'AlII1', 'dep', bAlII)

      endif

!---------------- SILICON NLTE DEPARTURES -------------------------

      if (nlte(14)) then

         do n = 1, 6

            call nlte_lev(nrhox, 'SiI'//str(n), 'dep', val)

            bSiI(n, 1 : nrhox) = val(1 : nrhox)

         enddo

         call nlte_lev(nrhox, 'SiII1', 'dep', bSiII)

      endif

!---------------- IRON NLTE DEPARTURES -------------------------

      if (nlte(26)) then

         do n = 1, 5

            call nlte_lev(nrhox, 'FeI'//str(n), 'dep', val)

            bFeI(n, 1 : nrhox) = val(1 : nrhox)

         enddo

         call nlte_lev(nrhox, 'FeII1', 'dep', bFeII)

      endif

!-----------------------------------------------------------------

      end

      subroutine nlte_lev(nrhox, levname, mode, val)

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

      un = 1845

      open(unit = un, file = 'nltepop/'//levname, action = 'read')

      read(un, *) s

      read(un, *) (val1(j), val2(j), val3(j), val4(j), val5(j),
     $             j = 1, nrhox)

      close(un)

      if (mode == 'pop') val(1 : nrhox) = val4(1 : nrhox)

      if (mode == 'dep') val(1 : nrhox) = val5(1 : nrhox)

      if (mode /= 'pop' .and. mode /= 'dep')
     $   stop 'subroutine nlte_lev: mode is not recognized'

      deallocate(val1)
      deallocate(val2)
      deallocate(val3)
      deallocate(val4)
      deallocate(val5)

      return

      end

      end module
