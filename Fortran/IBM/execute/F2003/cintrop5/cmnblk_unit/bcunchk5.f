! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/unextchk.sh bcunchk5 comain1
! %COMPOPTS: -qfree=f90 -qextchk
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : bcunchk5.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : July 28, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common blocks work with
!*                              : -qextchk
!*                              :
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  06/28/04    KV    - corrected incorrect ineroperability of "character(1) z"
!*                      with "char z[1]" - defect 285281
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

subroutine fsub()
  implicit none

  integer x
  real y
  character z

  common /blk/ x, y, z
  bind(c) :: /blk/

  print *, x, y, z
  x = 4
  y = 5.0
  z = 'F'
  print *, x, y, z

end subroutine
