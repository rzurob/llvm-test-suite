! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					   dummy argument defined to be abstract type.
!*                                         polymorphic abstract type dummy argument (non-pointer and non-allocatable) with array
!*                                           a)polymorphic abstract type actual argument
!*                                           b)polymorphic extension type of abstract type actual argument
!*                                           c)non-polymorphic extension type of abstract type actual argument
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
      real :: rid
   end type

contains

   subroutine foo(a, b)
      class(base) :: a(2,*)
      class(base) :: b(2)

      if (a(1,1)%id .ne. 3)   error stop 1_4
      if (a(2,1)%id .ne. 6)   error stop 2_4

      if (b(1)%id .ne. 3)   error stop 3_4
      if (b(2)%id .ne. 6)   error stop 4_4

   end subroutine

   integer function boo(a)
      class(base) :: a(:)
      boo = a(1)%id + a(2)%id
   end function

end module

program dummy015
   use m

   class(base), allocatable :: b1(:)
   type(child), allocatable, target :: c1(:)
   class(child), pointer :: c2(:)

   allocate (b1(2), source = (/ child(3,4.5), child(6,7.8) /))
   allocate (c1(2), source = (/ child(3,4.5), child(6,7.8) /))

   c2 => c1

   call foo(b1,b1)
   call foo(c1,c1)
   call foo(c2,c2)

   if ( boo(b1) .ne. 9 ) error stop 5_4
   if ( boo(c1) .ne. 9 ) error stop 6_4
   if ( boo(c2) .ne. 9 ) error stop 7_4

end program