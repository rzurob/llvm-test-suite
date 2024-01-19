! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-12 (original: 04/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (very basic test for intent(out)
!*                               finalization; followed by default
!*                               initialization)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1,kbase_2) ! kbase_1,kbase_2=4,4
       integer, kind :: kbase_1,kbase_2
        integer(kbase_1), pointer :: data
        integer(kbase_2) :: id = 0

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4,4)), intent(inout) :: b ! tcx: (4,4)

        b%id = -1
        if (associated(b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal513akk
use m
    type (base(4,4)):: b1 ! tcx: (4,4)

    allocate (b1%data)
    b1%id = 100

    call abc (b1)

    if (associated (b1%data)) error stop 101_4
    if (b1%id /= 0) error stop 2_4

    allocate (b1%data)
    b1%id = 10

    call cba (b1)

    if (associated (b1%data)) error stop 3_4
    if (b1%id /= 0) error stop 4_4

    contains

    subroutine abc (b)
        type (base(4,4)), intent(out) :: b ! tcx: (4,4)
    end subroutine

    subroutine cba (b)
        class (base(4,4)), intent(out) :: b ! tcx: (4,4)
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2) to invoke with (4,4) / declare with (4,4) - 4 changes
