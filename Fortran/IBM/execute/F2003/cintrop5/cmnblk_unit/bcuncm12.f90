! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/uncs.sh bcuncm12 comc2
! %COMPOPTS: -qfree=f90
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
!* TEST CASE TITLE              : bcuncm12.f
!
!* PROGRAMMER                   : Helen Li
!* DATE                         : May. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test bind(c) common blocks in module
!*                              : work as global variables.
!*                              :
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  07/19/04    KV    - corrected error of "character(1) x" trying to be 
!*                      ineroperable with "char x[1]" (defect 285281)
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  integer x 
  real y
  character z(1)
  common /blk/ x, y, z
  bind(c, name="bar") :: /blk/
end module
program bcuncm12
  use mod
  x = 1
  y = 2.0
  z = 'F'
  print *, x, y, z
  call csub()
  print *, x, y, z
end program
