! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : elementalfinal.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : 1.FROM and TO are unlimited polymorphic,
!*                               2.TO is finalized , elemental final subroutine 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A 
 	integer, pointer :: iP
        contains
            final :: final1
    end type 

    type, extends(A) ::  B
        contains
            final :: final2
    end type 

    contains
        elemental subroutine final1(arg)
            type(A), intent(inout) :: arg
            arg%ip = 99 
        end subroutine
        elemental subroutine final2(arg)
            type(B), intent(inout) :: arg
            arg%ip = 88 
        end subroutine
end module


program main
use m

    class(*), allocatable :: aA(:)
    class(*), allocatable :: aB(:)

    integer, target :: t(4) =  (/ -12, -24, -36, -48 /)
 
    allocate(A :: aA(4))

    select type ( aA)
        type is (A)
	    aA(1)%ip => t(1)
	    aA(2)%ip => t(2)
	    aA(3)%ip => t(3)
	    aA(4)%ip => t(4)
    end select 

    allocate(aB(3), source= (/ b(t(1)),  B(t(2)), B(t(3)) /) )

    call move_alloc(aB, aA)

    if ( allocated(aB) ) stop 11
    if ( .not. allocated(aA)) stop 13
 
    select type ( aA)
        type is (B)
            if ( size(aA) /= 3 ) stop 21
	    if (aA(1)%ip /= 99 ) stop 23 
	    if (aA(2)%ip /= 99 ) stop 25
	    if (aA(3)%ip /= 99 ) stop 27
	class default
	    stop 31
    end select 

   end
