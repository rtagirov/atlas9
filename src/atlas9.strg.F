      module string_operations

      implicit none

      contains

      function str(int_num) result(str_num)

      integer, intent(in) :: int_num

      character (len = 10) :: str_num

!      IF (IntNum .LE. 9) THEN

!         WRITE(CharNum, '(I1)') IntNum

!         CharNum = '00'//CharNum

!      ELSEIF (IntNum .GE. 10 .AND. IntNum .LE. 99) THEN

!         WRITE(CharNum, '(I2)') IntNum

!         CharNum = '0'//CharNum

!      ELSEIF (IntNum .ge. 100 .and. IntNum .le. 999) THEN

!         WRITE(CharNum, '(I3)') IntNum

!      else

!         stop, 'module string_operations, function int_to_char: integer too large. abort'

!      ENDIF

      write(str_num, '(I10)') int_num

      str_num = trim(adjustl(str_num))

      return

      end function


      FUNCTION RM_CHAR(in_str, target_char) RESULT(out_str)

      CHARACTER(*), INTENT(IN) :: in_str

      CHARACTER :: target_char

      CHARACTER(:), ALLOCATABLE :: out_str

      CHARACTER :: ch

      INTEGER :: j

      out_str = ' '

      DO j = 1, LEN_TRIM(in_str)

         ch = in_str(j : j)

         IF (ch .NE. target_char) out_str = TRIM(out_str) // ch

      ENDDO

      out_str = TRIM(ADJUSTL(out_str))

      RETURN

      END FUNCTION RM_CHAR

      end module
