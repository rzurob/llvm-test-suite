! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO has defer type parameter
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type A (l)
        integer, len :: l
        character(l), allocatable :: ch
   end type

end module

use m

   type(A(5)), allocatable :: a2
   class(A(:)), allocatable :: a1

   allocate(a2, source = A(5)('abcde'))
   call move_alloc(a2, a1)

   if ( allocated(a2) ) error stop 11

   select type (a1)
	type is (A(*))
	    if ( a1%ch /= 'abcde' ) error stop 21
        class default
            stop 23
   end select
end
