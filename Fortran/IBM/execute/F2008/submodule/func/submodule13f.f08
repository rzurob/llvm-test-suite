!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : April 20, 2013
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 SUBMODULE BLOCK
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*  based on F2008/block/BInInt2ModProc.f
!*
!*  Define a block in a module subroutine, testing compatibility with the F2008
!*   BLOCK feature
!*
!*  Secondary tests:
!*  - nested subroutine definition, with block defined in the nested subroutine
!*
!*  Verify that the results match the values of the original test case.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dataMod
  integer(2), parameter :: pat1 = z'1234', pat2 = z'5678'
  integer(4), parameter :: pat3 = z'12345678'

end module

module mod

  interface

    module integer(4) function fun(e1, a2)
      integer(2), intent(in) :: e1, a2
    end function

  end interface

contains

  subroutine sub(e1, a2, e3)
    integer(2), intent(in) :: e1, a2
    integer(4), intent(out) :: e3

    call inner(e1, e3)

    contains

      subroutine inner(a1, a3)
        integer(2), intent(in) :: a1
        integer(4), intent(out) :: a3
        block
          use dataMod
          a3 = ior(ishft(int(a1,4),16),int(a2,4))
          print '("in sub:",i6,"|",i6,"=",i12,"; x",z4,"0000|x",z4,"=x",z8)', a1, a2, a3, a1, a2, a3
          print *, a1, a2, a3, pat1, pat2, a3
        end block
      end subroutine inner

  end subroutine sub


end module mod

submodule (mod) submod

contains

  integer(4) module function fun(e1, a2)

    integer(2), intent(in) :: e1, a2

    fun = inner(e1)

    contains

      module integer function inner(a1)
        integer(2), intent(in) :: a1
        block
          inner = ior(ishft(int(a1,4),16),int(a2,4))
        end block
      end function inner

    end function fun

end submodule submod


program BInInt2ModProc

  use :: mod
  use dataMod
  implicit none

  integer(4) :: i4, i4a

  print '("bs",z8)', ior(ishft(int(pat1,4),16),int(pat2,4))
  call sub(pat1, pat2, i4)
  if (i4 /= pat3) error stop 2

  print '("fun:x",z8)', fun(pat1, pat2)
  i4a = fun(pat1, pat2)
  if (i4a /= pat3 .or. fun(pat1,pat2) /= pat3) error stop 3

end program BInInt2ModProc