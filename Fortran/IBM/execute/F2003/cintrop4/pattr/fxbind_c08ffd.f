! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c08ffd  cxbind_c08ffd
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
!*
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     : Interoperable Functions.
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: The fortran procedure and a C function simultaneously
!*           hold a file open for I/O. Using  the fflush function in
!*           C code in order to ensure that the data  read from
!*           or write to a fully-buffered stream shows up right away
!*
!*           Main program in C, call Fortran Subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine  openfile () bind(c)
  use assertmod
  IMPLICIT NONE
  character ch
  logical test

  OPEN(UNIT=7, FILE='test.txt')

    READ (UNIT=7, FMT=200) ch
  200 FORMAT (1A)

    test = ch .eq. 'I'
    call assert(test,'Hello, the result is not correct!',22)

END subroutine  openfile
