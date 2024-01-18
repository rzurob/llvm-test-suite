! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal004a3k
!*
!*  DATE                       : 2007-10-31 (original: 02/08/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final subroutine (final sub using assumed-size
!                               array as the dummy-arg)
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

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(4)), intent(in) :: b(*) ! tcx: (4)

        print *, 'finalizeBaseArray Rank 1'
        print *, 'first 2 elements:', b(1:2)%id
    end subroutine

    subroutine finalizeBaseArray2 (b)
        type(base(4)), intent(in) :: b(3,*) ! tcx: (4)

        print *, 'finalizeBaseArray Rank 2'
        print *, 'first 3 elements:', b(:,1)%id
    end subroutine
end module

program ffinal004a3k
use m
    class (base(4)), pointer :: b0, b1(:), b2(:,:) ! tcx: (4)

    allocate (b0, b1(2), b2(2,2))

    b0%id = 1
    b1%id = (/10, 20/)
    b2%id = reshape ((/100, 200, 300, 400/), (/2,2/))

    deallocate (b0)
    deallocate (b1)
    deallocate (b2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
