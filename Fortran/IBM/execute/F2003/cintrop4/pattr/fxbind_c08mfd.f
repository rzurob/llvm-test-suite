! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c08mfd  cxbind_c08mfd
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
!* TEST CASE TITLE              : fxbind_c08mfd
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : Interoperable Functions contained 
!*                                in module.
!*                               
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
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
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module check
contains 
  subroutine  openfile () bind(c)
    use assertmod
    IMPLICIT NONE
    character ch
    logical test

    OPEN(UNIT=7, FILE='test.txt')

    READ (UNIT=7, FMT=200) ch
    PRINT *, 'The character  is ', ch
200 FORMAT (1A)

    test = ch .eq. 'I'
    call assert(test,'Hello, the result is not correct!',22)
  END subroutine  openfile

end module check
