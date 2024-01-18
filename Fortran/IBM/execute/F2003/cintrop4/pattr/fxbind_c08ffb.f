! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c08ffb  cxbind_c08ffb
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c08ffb
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran programs interoperate with C functions
!*                                through a Fortran procedure interface that uses
!*                                the BIND specification .
!*            
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - main written in Fortran, Fortran  calls C Function.
!*   - The interoperable  procedure itself is  implemented using C
!*     Function.
!*   - Test BIND(C) attribute with I/O in External Fortran Procedure.
!*
!*  
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM FILE2
  use  ASSERTMOD
  IMPLICIT NONE
  INTERFACE  ! interface equivalent to C function prototype `void create_file (void);'
     SUBROUTINE create_file () BIND(C)
     END SUBROUTINE create_file
  END INTERFACE

  INTEGER SUM, I, NUMBER
  integer, parameter :: SHORT_STRING_LEN = 64
  integer, parameter ::LONG_STRING_LEN=200
  integer, parameter ::stdout=6
  logical :: lexist               ! True if file exists
  character (len=LONG_STRING_LEN) command
  character (len=SHORT_STRING_LEN) start_name, old_name
  start_name = "fxbind_c08ffb.dat"
  old_name = "fxbind_c08ffb.bak"
  ! Does file already exist?
  inquire (FILE=start_name, EXIST=lexist)

  ! if the file exist ,  saving the old one under a new name.
  
  if (lexist) then
     command = 'mv' // ' ' // start_name // ' ' // old_name
     call system (command)
  end if

  SUM = 0

  ! Call C subroutine to create the file 
  !  containing ten one or two digits  numbers, each on a different line.

  call  create_file
  OPEN(UNIT=7, FILE=start_name)

  DO I = 1, 10
     READ (UNIT=7, FMT=200) NUMBER
     SUM = SUM + NUMBER
  ENDDO
  200 FORMAT (I2)
  call assert((sum == 55),'Hello, the sums are not correct !',9)
  STOP
END PROGRAM FILE2
