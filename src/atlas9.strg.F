      module strings

      implicit none

      contains

      function int_to_char(IntNum) RESULT(CharNum)

      INTEGER, INTENT(IN) :: IntNum

      CHARACTER (len = 10) :: CharNum

!      IF (IntNum .LE. 9) THEN

!         WRITE(CharNum, '(I1)') IntNum

!         CharNum = '00'//CharNum

!      ELSEIF (IntNum .GE. 10 .AND. IntNum .LE. 99) THEN

!         WRITE(CharNum, '(I2)') IntNum

!         CharNum = '0'//CharNum

!      ELSEIF (IntNum .ge. 100 .and. IntNum .le. 999) THEN

!         WRITE(CharNum, '(I3)') IntNum

!      else

!         stop, 'module strings, function int_to_char: integer too large. abort'

!      ENDIF

      WRITE(CharNum, '(I10)') IntNum

      CharNum = TRIM(ADJUSTL(CharNum))

      return

      end function int_to_char


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
