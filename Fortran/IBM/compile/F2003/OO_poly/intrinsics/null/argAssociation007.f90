!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! PROGRAMMER                 : Yong Du
! DATE                       : 03/04/2005
! PRIMARY FUNCTIONS TESTED   : null
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Diagnostic test case. null is an actual
!                              argument without MOLD, and dummy argument
!                              is with assumed character length.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation007
use m
    character(8), pointer :: c1
    character(LEN=12), allocatable :: c2

    allocate(c1, SOURCE="c one")
    allocate(c2, SOURCE="c two")

    call sub1(null(c1), null())

    contains

    subroutine sub1(arg1, arg2)
        character(LEN=*), pointer :: arg1
        character(*), allocatable :: arg2

        if(associated(arg1)) error stop 1_4
        if(allocated(arg2)) error stop 2_4
    end subroutine
end
