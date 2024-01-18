! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : multiPntAssn.f 
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
!*  DESCRIPTION                : After executing multi pointer assignments 
!*                               test if TO is associated with the pointer in 
!*                               the last pointer assignemnts
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type A 
       integer id 
   end type

   type, extends(A) :: B
   end type 

   type, extends(B) :: C  
    end type

end module

use m

    type(C), target, allocatable :: c1
    class(A), target, allocatable :: a1

    class(A), pointer :: p1
    class(B), pointer :: p2
    type(C), pointer :: p3
    class(*), pointer :: p0

    allocate(c1, source = C(99))
    allocate(a1, source = A(8))

    p3 => c1 
    p2 => p3
    p1 => p2
    p0 => p1

    call move_alloc(c1, a1)
  
    if ( allocated(c1) ) stop 11
    if ( .not. allocated(a1) ) stop 13
 
    if ( .not. associated (p0, a1 )) stop 23
    
    select type (p0)
        type is (C)
            if ( p0%id /= 99 ) stop 25
        class default
            stop 33
    end select

end
