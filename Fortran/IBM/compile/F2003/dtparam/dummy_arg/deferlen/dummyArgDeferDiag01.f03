!*********************************************************************
!*  ===================================================================
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
!*  1. An actual argument associated with a dummy argument that is allocatable or a pointer shall have deferred the same type parameters as the dummy argument.
!*  2. following is illegal case when deferred type parameters for dummy argument and actual argument don't match
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k,l)
    integer,kind :: k=2
    integer,len  :: l=3
  end type
  contains
    subroutine sub1(arg)
       type(dtp(2,3)),allocatable :: arg
    end subroutine
    subroutine sub2(arg)
       type(dtp(2,3)),pointer :: arg
    end subroutine
end module

program dummyArgDeferDiag01

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

  type(dtp(2,4)),allocatable :: dtp1
  type(dtp(2,4)),pointer     :: dtp2=>null()
  type(dtp(2,:)),allocatable :: dtp3
  type(dtp(2,:)),pointer     :: dtp4=>null()

  call sub1(dtp1)
  call sub2(dtp2)
  call sub1(dtp3)
  call sub2(dtp4)

  allocate(dtp3,source=dtp(2,3)())
  allocate(dtp4,source=dtp(2,3)())

  call sub1(dtp3)
  call sub2(dtp4)

  call sub3(dtp1)
  call sub4(dtp2)

  call sub5(dtp1)
  call sub6(dtp2)

  call sub1(fun1(dtp1))
  call sub2(fun2(dtp2))

  contains

  subroutine sub5(arg)
    type(dtp(2,:)),allocatable :: arg
  end subroutine

  subroutine sub6(arg)
    type(dtp(2,:)),pointer :: arg
  end subroutine

  function fun1(arg)
    type(dtp(2,*)),intent(in)  :: arg
    type(dtp(2,:)),allocatable :: fun1
    fun1=arg
  end function

  function fun2(arg)
    type(dtp(2,*)),target,intent(in)  :: arg
    type(dtp(2,:)),pointer :: fun2
    fun2=>arg
  end function

end program

subroutine sub3(arg)
  use m
  type(dtp(2,:)),allocatable :: arg
end subroutine

subroutine sub4(arg)
  use m
  type(dtp(2,:)),pointer :: arg
end subroutine
