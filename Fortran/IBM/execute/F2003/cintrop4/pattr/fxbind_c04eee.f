! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan, 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran Entry in function called from C
!*                              - Interop procedure in module.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  derived type.
!*   - The interoperable  procedure itself is implemented using Fortran
!*     function Entry Statement.
!*    - primary entry point have bind(c) attribute  and an alternate
!*     entry point do not have bind(c) attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in C, C calls FORTRAN functions.
!*
!*  ALGORITHM :
!*          1. C program call the Fortran function has a primary entry
!*             point and an alternate entry point.
!*          2. Assertion: Check the return value in C
!*             to verify it is correct.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mderived
contains
FUNCTION swap_drt(x,y) bind(c)
  use iso_c_binding
  type,bind(c) :: ROBOT
     real   velocity
     integer energy
     integer  IQ
     character name
  end type ROBOT

  integer :: swap_drt,ent_swap_drt
  type(ROBOT):: x,y
  x =y
  swap_drt=x%IQ
  return

  entry ent_swap_drt (x) bind(c)

  x%velocity = x%velocity + 10.0
  x%IQ = x%IQ + 10
  x%energy = x%energy + 10
  x%name = 'U'
  ent_swap_drt = x%IQ

  return
end FUNCTION swap_drt
end module mderived