! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : July 28, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test bind(c) common blocks work with
!*                              : -qextchk
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
