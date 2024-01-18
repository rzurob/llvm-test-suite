! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : poly4ToFmfinal.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : 1. FROM and TO are of poly type 
!*                               2. rank-1
!*                               3. TO is finalized 
!*		                 4. FROM is zero-size array  
!*                        
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer  :: numA = 0, numB = 0 

    type A
	integer :: iA
	contains
	    final :: finalA 
    end type

    type, extends(A) :: B
	integer :: iB
        type(A) :: x
        contains
            final :: finalB
    end type

    contains
        subroutine finalA(a)
            type(A), intent(in) :: a
            numA = numA + 1
        end subroutine

        subroutine finalB(a)
	    type(B), intent(in) :: a
            numB = numB + 1 
        end subroutine 
end module


program main
use m

    class(A), allocatable :: to(:)
    class(B), allocatable :: from(:)

    allocate(B :: to(1))
    allocate(B :: from (0:-1) )
 
    select type (to)
        type is (B)
	    to(1)%iB = 1003
            to(1)%iA = 1002
            to(1)%x%iA = 1001

            to(1) = B(iA = 101, iB = 102, x= A(11)) 
    end select
 
    if ( numA /= 5 ) stop 11
    if ( numB /= 2 ) stop 13

    ! to is rank-1 array
    call move_alloc(from ,to ) 

    if ( .not. allocated(to) ) stop 21
    if ( allocated(from) ) stop 23

    select type (to)
	type is (B)
            if ( size(to) /= 0 ) stop 31
    end select 

    if ( numA /= 7 ) stop 41
    if ( numB /= 2 ) stop 43 

   end

