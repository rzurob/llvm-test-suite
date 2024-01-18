! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ulmTOpolyPntDTFrmcmp.f 
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
!*  DESCRIPTION                : 1.TO is of type class(*), a component of DT 
!*                               2.FROM and pointer have the same declared type
!*                               3. FROM is a component of DT, non-poly 
!*                               4. pointer is var, poly
!*                               defect 322504 322514
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: a1(:)
    end type
end module

program main
use m

    type, extends(base) :: child 
        type(base), allocatable :: b1 (:)
    end type

    type(child), target :: c1
    type(base), allocatable :: ll(:)
    class(base), pointer  :: p(:)

    allocate(ll(2), source = (/ base( (/21, 32 /) ), base( (/56, 67/))  /))
  
    c1 = child( a1= null(), b1 = ll )

    p => c1%b1

    call move_alloc(c1%b1, c1%a1)

    if ( allocated (c1%b1) ) stop 11
    if ( .not. allocated(c1%a1)) stop 13

    select type ( p )
        type is (base)
            select type ( x => p(1)%a1 )
                type is (integer)
                    if ( x(1) /= 21 ) stop 21
                    if ( x(2) /= 32 ) stop 23 
                class default
                    stop 31
            end select
            select type ( x => p(2)%a1 )
                type is (integer)
                    if ( x(1) /= 56 ) stop 25
                    if ( x(2) /= 67 ) stop 27 
                class default
                    stop 41
            end select
        class default 
            stop 43
    end select

    select type ( x=> c1%a1)
        type is (base)
            if ( .not. associated(p, x)) stop 61 
	class default
            stop 63
    end select

    end
