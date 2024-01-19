! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 06/21/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (poly-pointer's finalization)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (kChild) ! kChild=8
       integer, kind :: kChild
        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,8)) :: c ! tcx: (4,8)
        print *, 'finalizeChild', c%id
    end subroutine
end module

program ffinal014akk
use m
    type (base(4)), pointer :: b_ptr ! tcx: (4)
    type (child(4,8)), pointer :: c_ptr ! tcx: (4,8)

    class (base(4)), pointer :: b ! tcx: (4)

    allocate (b_ptr, c_ptr)

    b_ptr%id = 1
    c_ptr%id = 10

    b => c_ptr

    deallocate (b)

    print *, 're-assign pointer b'

    b => b_ptr

    deallocate (b)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (kChild) to invoke with (4,8) / declare with (4,8) - 2 changes
