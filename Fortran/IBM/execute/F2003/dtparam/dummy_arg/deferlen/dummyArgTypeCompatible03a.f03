!*********************************************************************
!*  ===================================================================
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
!*         dummy  : Pbase
!*  sub3 - actual : Pbase
!*         dummy  : Pbase
!*  sub4 - actual : Pbase
!*         dummy  : Tbase
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
       class(base(2,*)),intent(inout) :: arg

       print *,"in sub2"
       call sub3(arg)

    end subroutine

    subroutine sub3(arg)
       class(base(2,*)),intent(inout) :: arg

       print *,"in sub3"
       call sub4(arg)
    end subroutine

    subroutine sub4(arg)
       type(base(2,*)),intent(inout)  :: arg

       print *,"in sub4"
       arg%i1=99
    end subroutine

end module

program dummyArgTypeCompatible03a
  use m
  implicit none

  class(child(2,:,4,:)),allocatable :: dtp1

  call sub1(dtp1)
  select type(dtp1)
     class is(child(2,*,4,*))
        print *,"type is child"
        print *,dtp1%i1,dtp1%i2
     class default
        error stop 10_4
  end select

end program