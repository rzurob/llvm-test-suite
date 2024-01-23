! *********************************************************************
! cinterop/pattr/fxbind_c03aaz.f, xlftest.cinterop, tstdev.cinterop, 1.1
! Extract Date/Time: 04/04/06 11:11:54
! Checkin Date/Time: 04/03/23 17:32:43
! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan 7, 2004
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran Entry in function called from C
!*
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with character*1.
!*   - The interoperable  procedure itself is  implemented using Fortran
!*     function Entry Statement.
!*   - The primary entry point do not have bind(c) attribute while
!*     the alternate entry point have.
!*   - main written in C, C  calls FORTRAN functions.
!*  ALGORITHM :
!*          1. C program call the Fortran function has a primary entry
!*             point and an alternate entry point.
!*          2. Assertion: Check the return value  in C
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

function fn_char(a, b) bind(c)
  character(1) :: a
  character(1),value :: b
  character(1)             :: fn_char
  character(1)             :: char_val
  fn_char = b
  return
  entry ent_char()result(char_val) bind(c)
  char_val = 'K'
end function fn_char

