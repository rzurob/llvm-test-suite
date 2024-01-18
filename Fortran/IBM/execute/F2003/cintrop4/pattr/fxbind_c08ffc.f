! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions with
!*                               I/O operation.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: The fortran subroutine write the numbers 1 through N
!*     to a file,call the flush_ procedure to save any buffer
!*     for that file. Write Formatted data in Fortran and read
!*     formatted data in C.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

SUBROUTINE creat_file () BIND(C)
  USE XLFUTILITY

  IMPLICIT NONE
  INTEGER I, NUMBER
  integer, parameter :: SHORT_STRING_LEN = 50
  character (len=SHORT_STRING_LEN)  file_name
  logical :: lexist

  NUMBER = 20
  file_name ="fxbind_c08ffc.dat"

  INQUIRE ( FILE=file_name, EXIST=lexist )
  exists: IF ( .NOT. lexist ) THEN
     ! It's OK, the file didn't already exist.  Open file.
     OPEN (UNIT=8, FILE=file_name, STATUS='NEW', ACTION='WRITE')

  ELSE
     ! File exists.  Should replace it.
     WRITE (*,*) 'Output file exists.  Overwrite it! '

     ! It's OK.  Open file.
     OPEN (UNIT=8, FILE=file_name, STATUS='REPLACE', ACTION='WRITE')

  END IF exists

  DO I = 1, NUMBER
     WRITE (UNIT=8, FMT=200) I

  ENDDO
  CALL FLUSH_(8)
200 FORMAT (I2)

  STOP

END SUBROUTINE creat_file
