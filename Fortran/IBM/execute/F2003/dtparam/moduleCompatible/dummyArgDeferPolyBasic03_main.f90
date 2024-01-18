!*********************************************************************
!*  ===================================================================
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
