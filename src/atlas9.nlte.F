      module nlte

      implicit none

      logical, parameter :: nltepop = .true.
!      logical, parameter :: nltepop = .false.

!      real*8, allocatable, dimension(:) :: bhii

      contains

      subroutine read_nlte_lev(nrhox, levname, mode, val)

      implicit none

      integer, intent(in) :: nrhox

      character (len = *), intent(in) :: levname

      character (len = 3), intent(in) :: mode

      real*8, dimension(nrhox), intent(out) :: val

      real*8, dimension(nrhox) :: val1, val2, val3, val4, val5

      character (len = 10000) :: s

      integer :: un, j

      un = 1845

      open(unit = un, file = 'nltepop/'//levname, status = 'old')

      read(un, *) s

      read(un, *) (val1(j), val2(j), val3(j), val4(j), val5(j),
     $             j = 1, nrhox)

      close(un)

      if (mode == 'pop') val(1 : nrhox) = val4(1 : nrhox)

      if (mode == 'dep') val(1 : nrhox) = val5(1 : nrhox)

      if (mode /= 'pop' .and. mode /= 'dep')
     $   stop 'subroutine read_nlte_lev: mode is not recognized'

      return

      end

      end module
