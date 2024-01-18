! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : deferTP4TO.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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

   if ( allocated(a2) ) stop 11

   select type (a1)
	type is (A(*))
	    if ( a1%ch /= 'abcde' ) stop 21 
        class default
            stop 23
   end select
end
