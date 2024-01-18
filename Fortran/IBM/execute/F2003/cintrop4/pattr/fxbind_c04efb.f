! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/run.sh fxbind_c04efb  cxbind_c04efb
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
!* PRIMARY FUNCTIONS TESTED     :Interoperable Functions.
!*                              - Fortran Entry in function called from C
!*                              - interop functions contained in Module.
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: BINC(C) attribute with  different intrinsic data type,
!*           real*4,real*8.
!*   - The interoperable  procedure itself is  implemented using Fortran
!*     function Entry Statement.
!*   - primary entry point have bind(c) attribute  and an alternate
!*     entry point do not have bind(c) attribute.
!*   - passing scalar arguments by REFERENCE and by VALUE
!*   - main written in C, C  calls FORTRAN functions.
!*
!*  ALGORITHM :
!*          1. C program call the Fortran function has a primary entry
!*            point and an alternate entry point.
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

module mint
contains

function fn_real4 (a, b) bind(c)
  real(4), intent(inout) :: a
  real(4), intent(inout) :: b
  real(4)             :: fn_real4
  real(4)             :: ent_real4
  fn_real4 = a * b
  a = a + 1.0e0
  b = b + 1.0e0
  return
  entry ent_real4 (a, b)
  a = a + 1.0e0
  b = b + 1.0e0
  ent_real4 = a + b
  return
end function fn_real4

function fn_real8 (a, b) bind(c)
  real(8), intent(inout) :: a
  real(8), intent(inout) :: b
  real(8)             :: fn_real8
  real(8)             :: ent_real8
  fn_real8 = a * b
  a = a + 1.0d0
  b = b + 1.0d0
  return
  entry ent_real8 (a, b)
  a = a + 1.0d0
  b = b + 1.0d0
  ent_real8 = a + b
  return
end function fn_real8

function fn_realval4 (a, b) bind(c)
  real(4), value :: a
  real(4), value :: b
  real(4)             :: fn_realval4
  real(4)             :: ent_realval4
  a = a +1.0e0
  b = b +1.0e0
  fn_realval4 = a * b
  return
  entry ent_realval4 (a, b)
  a = a +1.0e0
  b = b +1.0e0
  ent_realval4 = a + b
  return
end function fn_realval4

function fn_realval8 (a, b) bind(c)
  real(8), value :: a
  real(8), value :: b
  real(8)             :: fn_realval8
  real(8)             :: ent_realval8
  a = a +1.0d0
  b = b +1.0d0
  fn_realval8 = a * b
  return
  entry ent_realval8 (a, b)
  a = a +1.0d0
  b = b +1.0d0
  ent_realval8 = a + b
  return
end function fn_realval8

end module mint
