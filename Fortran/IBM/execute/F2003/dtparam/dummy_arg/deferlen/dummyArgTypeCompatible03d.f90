!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgTypeCompatible03d.f
!*
!*  DATE                       : Nov. 14 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!*  sub1 - actual : Pchild
!*         dummy  : Pchild
!*  sub2 - actual : Pchild
!*         dummy  : Pchild
!*  sub3 - actual : Pchild
!*         dummy  : Pchild
!*  sub4 - actual : Pchild
!*         dummy  : Tchild
!*   test actual argument pass through several layers of procedure,verify type,and component value
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
      integer(k1)  :: i1
  end type

  type,extends(base) :: child(k2,l2)
      integer,kind :: k2
      integer,len  :: l2
      integer(k2)  :: i2
  end type

  contains

    subroutine sub1(arg)

      class(child(2,:,4,:)),allocatable,intent(out) :: arg

      print *,"in sub1"

      allocate(child(2,3,4,5) :: arg)

      arg%i1=1
      arg%i2=-1
      call sub2(arg)

    end subroutine

    subroutine sub2(arg)
       class(child(2,*,4,*)),intent(inout) :: arg

       print *,"in sub2"
       call sub3(arg)
    end subroutine

    subroutine sub3(arg)
       class(child(2,*,4,*)),intent(inout) :: arg

       print *,"in sub3"

       call sub4(arg)
    end subroutine

    subroutine sub4(arg)
       type(child(2,*,4,*)),intent(inout)  :: arg

       print *,"in sub4"

       arg%i1=99
       arg%i2=-99

    end subroutine

end module

program dummyArgTypeCompatible03d
  use m
  implicit none

  class(child(2,:,4,:)),allocatable :: dtp1

  call sub1(dtp1)

  print *,dtp1%i1,dtp1%i2

end program
