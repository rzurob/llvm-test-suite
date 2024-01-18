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
!*                                         polymorphic abstract type dummy argument (non-pointer and non-allocatable) with
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
   end type

contains

   subroutine foo(a)
      class(base) :: a
      if (a%id .ne. 3)   error stop 1_4
   end subroutine

   integer function boo(a)
      class(base) :: a
      boo = a%id
   end function

end module

program dummy002
   use m

   class(base), allocatable :: b1
   type(child), target :: c1 = child(3)
   class(child), pointer :: c2

   allocate (b1, source = child(3))
   c2 => c1

   call foo(b1)
   call foo(c1)
   call foo(c2)

   if ( boo(b1) .ne. 3 ) error stop 2_4
   if ( boo(c1) .ne. 3 ) error stop 3_4
   if ( boo(c2) .ne. 3 ) error stop 4_4

end program