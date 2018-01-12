      module file_operations

      implicit none

      character (len=9), parameter :: atm_mod_file = 'atmod.inp'

      character (len=8), parameter :: nlte_file = 'DATOM_NLTE'

      character (len=8), parameter :: spec_file = 'spec.out'

      character (len=9), parameter :: print_file = 'print.out'

      public

      contains

      SUBROUTINE CLEAN_DIR(DIR)

      CHARACTER (LEN = *), INTENT(IN) :: DIR

      CHARACTER(:), ALLOCATABLE :: DIRECTORY

      DIRECTORY = TRIM(ADJUSTL(DIR))

      CALL SYSTEM('rm -vrf'//' '//DIRECTORY//'*')

      END SUBROUTINE


      SUBROUTINE MKDIR(DIR)

      CHARACTER (LEN = *), INTENT(IN) :: DIR

      CHARACTER(:), ALLOCATABLE :: DIRECTORY

      DIRECTORY = TRIM(ADJUSTL(DIR))

      CALL SYSTEM('mkdir -vp'//' '//DIRECTORY)

      END SUBROUTINE


      SUBROUTINE RM_FILE(FILE_NAME, OPTIONS)

      CHARACTER (LEN = *), INTENT(IN) ::           FILE_NAME

      CHARACTER (LEN = *), INTENT(IN), OPTIONAL :: OPTIONS

      CHARACTER(:), ALLOCATABLE ::                 FILENAME

      FILENAME = TRIM(ADJUSTL(FILE_NAME))

      CALL SYSTEM('rm '//OPTIONS//' '//FILENAME)

      END SUBROUTINE


      subroutine open_to_append(file_unit, file_name)

      character (len = *), intent(in) :: file_name

      integer, intent(in) ::             file_unit

      logical ::                         file_exists

!      SELECTCASE(FILE_NAME)

!            CASE('line_feautrier_matrix.out'); OPEN(UNIT = FILE_UNIT, FILE = FILE_NAME, FORM = 'FORMATTED'); RETURN

!            CASE('full_feautrier_matrix.out'); OPEN(UNIT = FILE_UNIT, FILE = FILE_NAME, FORM = 'FORMATTED'); RETURN

!            CASE DEFAULT
                
      inquire(file = file_name, exist = file_exists)

      if (.not. file_exists) open(unit = file_unit, file = file_name,
     $                            status = 'new', action = 'write')

      if (file_exists)       open(unit = file_unit, file = file_name,
     $                            status = 'old', action = 'write',
     $                            position = 'append')

!      ENDSELECT

      end subroutine open_to_append


      function num_of_lines(path_to_file) result(n)

      character (len = *), intent(in) :: path_to_file

      integer :: file_unit, n, io

      file_unit = 13745
     
      open(unit = file_unit, file = path_to_file, action = 'read')

      n = 0; io = 0

      do while (io .eq. 0)

         read(file_unit, *, iostat = io)

         if (io .ne. 0) exit

         n = n + 1

      enddo

      close(file_unit)

      return

      end function

      function num_of_columns(path_to_file) result(n)

      character (len = *), intent(in) :: path_to_file

      integer :: fu, n

      call system('head -1'//' '//path_to_file//' '//
     $            '| wc -w > temp.out')

      fu = 13745

      open(unit = fu, file = 'temp.out', action = 'read')
      
      read(fu, *) n

      close(fu)

      call system('rm temp.out')
     
      return

      end function

      function read_atm_file_col(col) result(array)

      integer, intent(in) ::               col

      real*8, dimension(:), allocatable :: array

      integer ::                           nol

      integer ::                           i, file_unit

      real*8, dimension(:), allocatable :: c1, c2, c3, c4, c5, c6, c7

      file_unit = 1834

      nol = num_of_lines(atm_mod_file)

      allocate(array(nol))

      allocate(c1(nol))
      allocate(c2(nol))
      allocate(c3(nol))
      allocate(c4(nol))
      allocate(c5(nol))
      allocate(c6(nol))
      allocate(c7(nol))

      open(unit = file_unit, file = atm_mod_file, action = 'read')

      selectcase(num_of_columns(atm_mod_file))

          case(4) ! MURAM format

           read(file_unit, *) (c1(i), c2(i), c3(i), c4(i), i = 1, nol)

          case(5) ! FAL format

           read(file_unit, *) 
     $     (c1(i), c2(i), c3(i), c4(i), c5(i), i = 1, nol)

          case(7) ! Kurucz format

           read(file_unit, *) 
     $     (c1(i), c2(i), c3(i), c4(i), c5(i), c6(i), c7(i), i = 1, nol)

          case default

            stop 'Function read_atm_file_col:
     $ Atmosphere model format is not recognized. Abort.'

      endselect

      close(file_unit)

      selectcase(col)

          case(1); array(1 : nol) = c1(1 : nol)
          case(2); array(1 : nol) = c2(1 : nol)
          case(3); array(1 : nol) = c3(1 : nol)
          case(4); array(1 : nol) = c4(1 : nol)
          case(5); array(1 : nol) = c5(1 : nol)
          case(6); array(1 : nol) = c6(1 : nol)
          case(7); array(1 : nol) = c7(1 : nol)

          case default; stop 'Function read_atm_file_col:
     $ col argument is not recognized. Abort.'

      endselect

      deallocate(c1, c2, c3, c4, c5, c6, c7)

      return

      end function read_atm_file_col

      end module file_operations
