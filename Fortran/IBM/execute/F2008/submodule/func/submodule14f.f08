!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : April 20, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 SUBMODULE  BLOCK
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*  based on F2008/block/BInModProc.f
!*
!*  Define a block in a module function, testing compatibility with the F2008
!*   BLOCK feature.
!*
!*  Secondary tests:
!*  - nested function definition, with block defined in the nested function
!*
!*  Verify that the results match the values of the original test case.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  integer(2), parameter :: pat1 = z'1234', pat2 = z'5678'
  integer(4), parameter :: pat3 = z'12345678'

  interface

     module subroutine sub(a1, a2, a3)
      integer(2), intent(in) :: a1, a2
      integer(4), intent(out) :: a3
    end subroutine sub

  end interface

contains

  integer(4) function fun(a1, a2)
    integer(2), intent(in) :: a1, a2
    block
      fun = ior(ishft(int(a1,4),16),int(a2,4))
    end block
  end function fun

end module mod

submodule (mod) submod

contains

  module subroutine sub(a1, a2, a3)
    integer(2), intent(in) :: a1, a2
    integer(4), intent(out) :: a3
    block
      a3 = ior(ishft(int(a1,4),16),int(a2,4))
      print '("in sub:",i6,"|",i6,"=",i12,"; x",z4,"0000|x",z4,"=x",z8)', a1, a2, a3, a1, a2, a3
      print *, a1, a2, a3, pat1, pat2, a3
    end block
  end subroutine sub

end submodule submod

program BInModProc

  use :: mod
  implicit none

  integer(4) :: i4, i4a

  print '("bs",z8)', ior(ishft(int(pat1,4),16),int(pat2,4))
  call sub(pat1, pat2, i4)
  if (i4 /= pat3) error stop 2

  print '("fun:x",z8)', fun(pat1, pat2)
  i4a = fun(pat1, pat2)
  if (i4a /= pat3 .or. fun(pat1,pat2) /= pat3) error stop 3

end program BInModProc
