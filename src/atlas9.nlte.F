      module nlte

      implicit none

      logical :: nltepop

      real*8, allocatable, dimension(:) :: bhii

      contains

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

      subroutine read_nlte_file()

      use file_operations

      integer :: un

      character (len = 3) :: str

      un = 1532

      open(unit = un, file = nlte_file, action = 'read')

      read(un, *) str

      if (trim(adjustl(str)) .eq. 'off') nltepop = .false.
      if (trim(adjustl(str)) .eq. 'on')  nltepop = .true.

      close(un)

      return

      end

      end module
