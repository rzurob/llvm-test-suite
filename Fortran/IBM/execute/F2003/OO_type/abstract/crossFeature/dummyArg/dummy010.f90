! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         c) OPTIONAL attribute with polymorphic abstract type (pointer or allocatable)
!*                                            2) if actual argument is associated, try
!*                                               i) polymorphic abstract type actual argument
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
      class(base) :: a
      class(base), optional, allocatable :: b

      if (a%id .ne. 3) error stop 1_4
      if (present(b) ) then
         if (b%id .ne. 3) error stop 2_4
      else
         error stop 3_4
      end if

   end subroutine

   integer function boo(a, b)
      class(base) :: a
      class(base), optional, pointer :: b
      if ( present(b) ) then
         boo = a%id + b%id
      else
         boo = 30
      end if
   end function

end module

program dummy010
   use m

   class(base), allocatable :: b1
   class(base), pointer :: b2
   type(child), target :: c1 = child(3,3.4)

   allocate (b1, source = child(3,2.3))
   allocate (b2, source = child(3,2.3))

   call foo(b1, b1)
   call foo(c1, b1)

   if ( boo(c1, b2) .ne. 6 ) error stop 4_4
   if ( boo(b1, b2) .ne. 6 ) error stop 5_4

end program
