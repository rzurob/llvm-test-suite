! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         OPTIONAL attribute with non-polymorphic abstract type (illegal) with array
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
      type(base), optional :: b(:)
   end subroutine

   integer function boo(a, b)
      class(base) :: a
      type(base), optional :: b(:)
      if (.not. present(b) ) then
         boo = 5
      end if
   end function

end module

program dummy021

end program