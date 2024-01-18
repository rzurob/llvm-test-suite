!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Assumed type object as the first argument of
!*    IS_CONTIGUOUS, LBOUND, UBOUND, PRESENT, SHAPE, RANK, SIZE and C_LOC
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  function mod_fnc1(a) result(res)
     type(*), optional :: a
     logical :: res

     res = present(a)
  end function mod_fnc1

  subroutine module_sub1(a,b,c)
     type(*), optional :: a
     type(*), target :: b(:)
     type(*) :: c(:)

     print*, present(a)
     print*, lbound(c)
     print*, ubound(c)
     print*, size(c)
     print*, is_contiguous(b)
     print*, is_contiguous(c)
     !print*, rank(c)
     print*, shape(c)
     print*, c_loc(b)
  end subroutine module_sub1
end module mod

program AssumedType04d
implicit none

contains

  logical function fnc1(a)
     type(*), optional :: a

     fnc1 = present(a)
  end function fnc1

  subroutine sub1(a,b,c)
     type(*), optional :: a
     type(*), target :: b(:)
     type(*) :: c(:)

     print*, present(a)
     print*, lbound(c)
     print*, ubound(c)
     print*, size(c)
     print*, is_contiguous(b)
     print*, is_contiguous(c)
     !print*, rank(c)
     print*, shape(c)
     print*, c_loc(b)
  end subroutine sub1
end program AssumedType04d
