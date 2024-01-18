! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         i-a) non-polymorphic abstract type being dummy argument with arrays	(illegal)
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
      type(base), dimension(:,:) :: a
      type(base), dimension(*)   :: b
   end subroutine

   integer function boo(a)
      type(base), dimension(4) :: a
      boo = 5
   end function

end module

program dummy014

end program