!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgTypeCompatible02a.f
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
!*  sub1 - actual : Pbase
!*         dummy  : Pbase
!*  sub2 - actual : Pbase
!*         dummy  : Pbase
!*  sub3 - actual : Pbase
!*         dummy  : Tbase
!*  sub4 - actual : Tbase
!*         dummy  : Pbase
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

      class(base(2,:)),allocatable,intent(out) :: arg

      print *,"in sub1"

      allocate(child(2,3,4,5) :: arg)

      arg%i1=1

      select type(arg)
          class is(child(2,*,4,*))
            print *,"type is child"
            arg%i2=-1
            print *,arg%i1,arg%i2
          type is(base(2,*))
            error stop 10_4
          class default
            error stop 50_4
      end select

      call sub2(arg)

    end subroutine

    subroutine sub2(arg)
       class(base(2,*)),intent(inout) :: arg

       print *,"in sub2"
       call sub3(arg)

    end subroutine

    subroutine sub3(arg)
       type(base(2,*)),intent(inout) :: arg

       print *,"in sub3"
       call sub4(arg)
    end subroutine

    subroutine sub4(arg)
       class(base(2,*)),intent(inout)  :: arg

       print *,"in sub4"
       select type(arg)
          type is(base(2,*))
             print *,"type is base"
             arg%i1=99
          type is(child(2,*,4,*))
             error stop 11_4
          class default
             error stop 51_4
       end select
    end subroutine

end module

program dummyArgTypeCompatible02a
  use m
  implicit none

  class(base(2,:)),allocatable :: dtp1

  call sub1(dtp1)

  select type(dtp1)
     type is(base(2,*))
        error stop 12_4
     type is(child(2,*,4,*))
        print *,"type is child"
        print *,dtp1%i1,dtp1%i2
     class default
        error stop 52_4
  end select

end program
