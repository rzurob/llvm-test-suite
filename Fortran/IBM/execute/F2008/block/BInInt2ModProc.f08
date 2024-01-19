!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : block in different contexts (internal of module)
!*
!*  DESCRIPTION
!*
!*  block in procedure internal to module procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module mod

  integer(2), parameter :: pat1 = z'1234', pat2 = z'5678'
  integer(4), parameter :: pat3 = z'12345678'

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
        a3 = ior(ishft(int(a1,4),16),int(a2,4))
        print '("in sub:",i6,"|",i6,"=",i12,"; x",z4,"0000|x",z4,"=x",z8)', a1, a2, a3, a1, a2, a3
        print *, a1, a2, a3, pat1, pat2, a3
      end block
    end subroutine inner

  end subroutine sub


  integer(4) function fun(e1, a2)

    integer(2), intent(in) :: e1, a2

    fun = inner(e1)

  contains

    integer function inner(a1)
      integer(2), intent(in) :: a1
      block
        inner = ior(ishft(int(a1,4),16),int(a2,4))
      end block
    end function inner

  end function fun

end module mod


program BInInt2ModProc

  use :: mod
  implicit none

  integer(4) :: i4, i4a

  print '("bs",z8)', ior(ishft(int(pat1,4),16),int(pat2,4))
  call sub(pat1, pat2, i4)
  if (i4 /= pat3) error stop 2

  print '("fun:x",z8)', fun(pat1, pat2)
  i4a = fun(pat1, pat2)
  if (i4a /= pat3 .or. fun(pat1,pat2) /= pat3) error stop 3

end program BInInt2ModProc
