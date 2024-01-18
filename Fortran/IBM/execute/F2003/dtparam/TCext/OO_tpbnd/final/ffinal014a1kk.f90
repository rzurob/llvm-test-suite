! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal014a1kk
!*
!*  DATE                       : 2007-10-31 (original: 04/26/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (poly-pointer's deallocation)
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

    end type

    type, extends(base) :: child (kBase) ! kBase=4
       integer, kind :: kBase
        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeChild (c)
        type (child(4,4)) :: c ! tcx: (4,4)
        print *, 'finalizeChild', c%id
    end subroutine
end module

program ffinal014a1kk
use m
    type (child(4,4)), pointer :: c_ptr ! tcx: (4,4)

    class (base(4)), pointer :: b ! tcx: (4)

    allocate (c_ptr)

    c_ptr%id = 10

    b => c_ptr

    deallocate (b)

end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: child - added parameters (kBase) to invoke with (4,4) / declare with (4,4) - 2 changes
