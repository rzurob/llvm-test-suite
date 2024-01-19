!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. dummy argument is intent(out) polymorphic allocatable with deferred length, modify actual argument through last level of procedure call , and verify actual arugment's type and component value
!* 2. extended type has integer array component
!* 3. actual argument is scalar
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
      integer,kind :: k1
      integer,len  :: l1
  end type

  type,extends(base) :: child(k2,l2)
      integer,kind :: k2
      integer,len  :: l2
      integer(k1+k2)  :: i1(l1:l2)
  end type

  contains

    subroutine sub1(arg)

      class(base(2,:)),allocatable,intent(out) :: arg

      call sub2(arg)


    end subroutine

    subroutine sub2(arg)

      class(base(2,:)),allocatable,intent(out) :: arg

      call sub3(arg)

    end subroutine

    subroutine sub3(arg)

      class(base(2,:)),allocatable,intent(out) :: arg

      allocate(child(2,3,2,5) :: arg)

      select type(arg)
         type is (child(2,*,2,*))
            arg%i1=[-1,-2,-3]
         type is (base(2,*))
            error stop 10_4
      end select
    end subroutine

end module

program dummyArgDeferPolyBasic01
  use m
  implicit none

  class(base(2,:)),allocatable :: base1

  call sub1(base1)

  select type(base1)
     type is(child(2,*,2,*))
          print *,"it is child"
          print *,base1%i1
     type is(base(2,*))
          error stop 11_4
  end select
end program
