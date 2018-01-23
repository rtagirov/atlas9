      module nlte_module

      implicit none

      character (len = 2), dimension(30) :: sym

      character (len = 4), parameter, dimension(10) :: stage = ['I',
     $                                                          'II',
     $                                                          'III',
     $                                                          'IV',
     $                                                          'V',
     $                                                          'VI',
     $                                                          'VII',
     $                                                          'VIII',
     $                                                          'IX',
     $                                                          'X']
     $ 

      logical, dimension(30) ::    nlte

      integer, dimension(30)    :: nis

      integer, dimension(30, 3) :: nl

      real*8, allocatable, dimension(:, :, :, :) :: dep

      real*8, allocatable, dimension(:) ::       bHII

      real*8, allocatable, dimension(:, :, :) :: bC
      real*8, allocatable, dimension(:, :, :) :: bMg
      real*8, allocatable, dimension(:, :, :) :: bAl
      real*8, allocatable, dimension(:, :, :) :: bSi
      real*8, allocatable, dimension(:, :, :) :: bFe

      contains

      subroutine nlte_elem(nrhox)

      use file_operations
      use utils

      integer, intent(in)       :: nrhox

      integer                   :: un, io, k

      character (len = 150)     :: l

      logical                   :: some_elements_are_in_nlte, innlte

      nlte(1 : 30) = .false.

      un = get_file_unit(1400)

      open(unit = un, file = 'istageinfo.inp', action = 'read')

      read(un, '(A2,4(2x,i2))') (sym(k),
     $                           nis(k),
     $                           nl(k, 1),
     $                           nl(k, 2),
     $                           nl(k, 3),
     $                           k = 1, 30)

      close(un)

      allocate(dep(30, maxval(nis), maxval(nl), nrhox))

      allocate(bHII(nrhox))

      allocate(bC (maxval(nis), maxval(nl(6, :)),  nrhox))
      allocate(bMg(maxval(nis), maxval(nl(12, :)), nrhox))
      allocate(bAl(maxval(nis), maxval(nl(13, :)), nrhox))
      allocate(bSi(maxval(nis), maxval(nl(14, :)), nrhox))
      allocate(bFe(maxval(nis), 48,                nrhox))
!      allocate(bFe(maxval(nis), maxval(nl(26, :)), nrhox))

      dep =  1.0d0

      bHII = 1.0d0

      bC   = 1.0d0
      bMg  = 1.0d0
      bAl  = 1.0d0
      bSi  = 1.0d0
      bFe  = 1.0d0

      inquire(file = nlte_file, exist = some_elements_are_in_nlte)

      if (some_elements_are_in_nlte) then

          un = get_file_unit(1500)

          open(unit = un, file = nlte_file, action = 'read')

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

      endif

      return

      end

      subroutine nlte_depart(nrhox)

      use string_operations

      include 'common.sizebl'
      include 'common.stateb'
      include 'common.depart'

      integer, intent(in) :: nrhox

      real*8, dimension(nrhox) :: val

      character (len = 50) :: lev

      integer :: k, i, l

      call read_nlte_lev(nrhox, 'ELECTR', 'pop', val)

      xne(1 : nrhox) = xnatom(1 : nrhox) * val(1 : nrhox)

!------------- READING NLTE DEPARTURES START ----------------------

      do k = 1, 30

         sym(k) = trim(adjustl(sym(k)))

         if (.not. nlte(k)) cycle

         do i = 1, nis(k)

            do l = 1, nl(k, i)

               lev = sym(k)//stage(i)//str(l)

               if (k == 1.and.i == 1) lev = 'HMINUS'
               if (k == 1.and.i == 2) lev = sym(k)//stage(i - 1)//str(l)
               if (k == 1.and.i == 3) lev = 'HII'

               call read_nlte_lev(nrhox, lev, 'dep', val)

               dep(k, i, l, 1 : nrhox) = val(1 : nrhox)

            enddo

         enddo

      enddo

!------------- ASSIGNING NLTE DEPARTURES -------------------

!                   HYDROGEN

      bmin(1 : nrhox) = dep(1, 1, 1, :)

      do l = 1, 8

         bhyd(1 : nrhox, l) = dep(1, 2, l, :)

      enddo

      bHII = dep(1, 3, 1, :)

!                   CARBON

      bC(1, 1 : nl(6, 1), :) = dep(6, 1, 1 : nl(6, 1), :)
      bC(2, 1 : nl(6, 2), :) = dep(6, 2, 1 : nl(6, 2), :)

!                  MAGNESIUM

      bMg(1, 1 : nl(12, 1), :) = dep(12, 1, 1 : nl(12, 1), :)
      bMg(2, 1 : nl(12, 2), :) = dep(12, 2, 1 : nl(12, 2), :)

!                  ALUMINUM

      bAl(1, 1 : nl(13, 1), :) = dep(13, 1, 1 : nl(13, 1), :)
      bAl(2, 1 : nl(13, 2), :) = dep(13, 2, 1 : nl(13, 2), :)

!                  SILICON

      bSi(1, 1 : nl(14, 1), :) = dep(14, 1, 1 : nl(14, 1), :)
      bSi(2, 1 : nl(14, 2), :) = dep(14, 2, 1 : nl(14, 2), :)

!                   IRON

!     first several levels of neutral iron (calculated in the NESSY part)
      bFe(1, 1 : nl(26, 1), :) = dep(26, 1, 1 : nl(26, 1), :)
      
!     remaining 43 levels of neutral iron (connection to the last neutral NLTE level)
!      forall(l = nl(26, 1) + 1 : 48) bFe(1, l, :) = bFe(1, nl(26, 1), :)

!     remaining 43 levels of neutral iron (connection to the first neutral NLTE level)
      forall(l = nl(26, 1) + 1 : 48) bFe(1, l, :) = bFe(1, 1, :)

      bFe(2, 1 : nl(26, 2), :) = dep(26, 2, 1 : nl(26, 2), :)

      end subroutine nlte_depart

      subroutine read_nlte_lev(nrhox, levname, mode, val)

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
     $   stop 'subroutine read_nlte_lev: mode is not recognized'

      deallocate(val1)
      deallocate(val2)
      deallocate(val3)
      deallocate(val4)
      deallocate(val5)

      return

      end

      end module
