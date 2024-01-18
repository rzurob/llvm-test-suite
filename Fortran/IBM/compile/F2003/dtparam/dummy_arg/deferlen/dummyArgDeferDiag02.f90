!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferDiag02.f
!*
!*  DATE                       : Nov. 4 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Dummy Argument with deferred length
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. If the dummy argument is a pointer, the actual argument shall be a pointer and nondeferred type parameters and ranks shall agree. if dummy argument is allocatable and nondeferred type parameters and ranks shall agree.
!*  2. Following are illegal cases when dummy argument is pointer or allocatable but actual argument is not.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
    integer,kind :: k=2
    integer,len  :: l=3
  end type
  contains
    subroutine sub1(arg)
       type(dtp(2,:)),allocatable :: arg
    end subroutine
    subroutine sub2(arg)
       type(dtp(2,:)),pointer :: arg
    end subroutine
end module

program dummyArgDeferDiag02

  use m
  implicit none

  interface

  subroutine sub3(arg)
     import
     type(dtp(2,:)),allocatable :: arg
  end subroutine

  subroutine sub4(arg)
     import
     type(dtp(2,:)),pointer :: arg
  end subroutine

  end interface

  type(dtp(2,3)) :: dtp1
  type(dtp(2,:)),allocatable :: dtp2
  type(dtp(2,:)),pointer     :: dtp3

  call sub1(dtp1)
  call sub2(dtp1)
  call sub3(dtp(2,3)())
  call sub4(dtp(2,3)())
  call sub5(dtp3)
  call sub6(dtp2)
  call sub5(dtp1)
  call sub6(dtp(2,3)())

  contains

  subroutine sub5(arg)
    type(dtp(2,:)),allocatable :: arg
  end subroutine

  subroutine sub6(arg)
    type(dtp(2,:)),pointer :: arg
  end subroutine

end program

subroutine sub3(arg)
  use m
  type(dtp(2,:)),allocatable :: arg
end subroutine

subroutine sub4(arg)
  use m
  type(dtp(2,:)),pointer :: arg
end subroutine
