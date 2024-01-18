!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyBasic03.f
!*
!*  DATE                       : Nov. 17 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. polymorphic allocatable derived type is array
!*  2. parent type and child type both have derived type component
!*  3. length type parameter of derived type component are expression.
!*  4. actual argument has been allocated to child type, dummy argument has intent(in) attribute, verify dummy argument result in last level of procedure call
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type nest(l)
     integer,len  :: l
     character(l) :: c
  end type
  type base(l1)
      integer,len  :: l1
      type(nest(2*l1+1)) :: nest1
  end type

  type,extends(base) :: child(l2)
      integer,len  :: l2
      type(nest(3*l2+1)) :: nest2
  end type

  contains

    subroutine sub1(arg)

      class(base(*)),intent(in) :: arg(:)
      call sub2(arg)
    end subroutine

    subroutine sub2(arg)

      class(base(*)),intent(in) :: arg(5:)

      select type(arg)
        class is(child(*,*))
          call sub3(arg)
        class default
          error stop 101_4
      end select

    end subroutine

    subroutine sub3(arg)

      type(child(*,*)),intent(in) :: arg(:)

      if(lbound(arg,1) /= 1)                 error stop 10_4
      if(ubound(arg,1) /= 2)                 error stop 11_4
      if(arg%l1 /= 3)                        error stop 12_4
      if(arg%l2 /= 4)                        error stop 13_4
      if(arg%nest1%l /= 7)                   error stop 14_4
      if(arg(1)%nest1%c /= "abcdefg")        error stop 15_4
      if(arg(2)%nest1%c /= "1234567")        error stop 16_4
      if(arg%nest2%l /= 13)                  error stop 17_4
      if(arg(1)%nest2%c /= "aabbccddeeffg")  error stop 18_4
      if(arg(2)%nest2%c /= "1122334455667")  error stop 19_4

    end subroutine

end module

program dummyArgDeferPolyBasic03
  use m
  implicit none

  class(base(3)),allocatable :: base1(:)

  allocate( child(3,4) :: base1(0:1))

  base1(0)%nest1 = nest(7)("abcdefg")
  base1(1)%nest1 = nest(7)("1234567")

  select type(base1)
      type is(child(*,*))
         base1(0)%nest2 = nest(13)("aabbccddeeffg")
         base1(1)%nest2 = nest(13)("1122334455667")
      class default
         error stop 100_4
  end select

  call sub1(base1)

end program
