! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO has assumed type parameter
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   type A (l)
        integer, len :: l
        character(l), allocatable :: ch
   end type

end module

use m

   class(A(5)), allocatable :: a2, a1

   allocate(a2, source = A(5)('abcde'))
   allocate(A(5) :: a1)

   call sub(a2, a1)

   if ( allocated(a2) ) error stop 11

   select type (a1)
        type is (A(*))
            if ( a1%ch /= 'abcde' ) error stop 21
        class default
            stop 23
   end select

   contains
       subroutine sub( arg1, arg2)
           class(A(5)), allocatable :: arg1
           class(A(*)), allocatable :: arg2

           call move_alloc(arg1, arg2)
       end subroutine
end
