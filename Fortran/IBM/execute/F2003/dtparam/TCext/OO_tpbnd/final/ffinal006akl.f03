! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of structure component
!                               of rank-two array)
!*
!*  KEYWORD(S)                 :
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
        integer(kbase_1) id

        contains

        final :: finalizeBase, finalizeBaseArray1, finalizeBaseArray2
    end type

    type dataType (kdataType_1,ldataType_1) ! kdataType_1,ldataType_1=4,2
       integer, kind :: kdataType_1
       integer, len :: ldataType_1
        type (base(kdataType_1)) :: data(ldataType_1,ldataType_1) ! tcx: (kdataType_1)
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeArray1'
    end subroutine

    subroutine finalizeBaseArray2 (b)
        type(base(4)), intent(in) :: b(:,:) ! tcx: (4)

        print *, 'finalizeBaseArray2'
    end subroutine
end module

program ffinal006akl
use m
    class (dataType(4,:)), pointer :: d1, d2(:), d3(:,:) ! tcx: (4,:)

    allocate (dataType(4,2)::d1, d2(2), d3(2,2)) ! tcx: dataType(4,2)

    print *, 'test 1'

    deallocate (d1)

    print *, 'test 2'

    deallocate (d2)

    print *, 'test 3'

    deallocate (d3)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: dataType - added parameters (kdataType_1,ldataType_1) to invoke with (4,2) / declare with (4,*) - 1 changes
