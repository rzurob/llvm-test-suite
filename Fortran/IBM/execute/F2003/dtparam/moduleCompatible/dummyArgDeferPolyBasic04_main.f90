!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferPolyBasic04.f
!*
!*  DATE                       : Nov. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. polymorphic allocatable derived type is array
!*  2. parent type and child type both have allocatable derived type component with deferred length parameter
!*  3. pass allocatable component in last procedure, and modify component value, and verify result of actual argument
!234567890123456789012345678901234567890123456789012345678901234567890

program dummyArgDeferPolyBasic04
  use m
  implicit none

  class(base(:)),allocatable :: base1(:)

  allocate(child(3,4) :: base1(0:1))

  allocate(base1(0)%nest1,source=nest(2*base1%l1+1)("abcdefg") )
  allocate(base1(1)%nest1,source=nest(2*base1%l1+1)("1234567") )
  select type(base1)
     type is(child(*,*))
        allocate(base1(0)%nest2,source=nest(3*base1%l2+1)("aabbccddeeffg") )
        allocate(base1(1)%nest2,source=nest(3*base1%l2+1)("1122334455667") )
     class default
        error stop 51_4
  end select

  call sub1(base1)

  select type(base1)
     type is(child(*,*))
       if(base1(0)%nest1%c /= "xyz")                 error stop 14_4
       if(base1(0)%nest2%c /= "xxxyyyzzz")           error stop 15_4
       if(base1(1)%nest1%c /= "789")                 error stop 16_4
       if(base1(1)%nest2%c /= "777888999")           error stop 17_4
     class default
       error stop 52_4
  end select

end program
