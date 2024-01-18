! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal513a5k
!*
!*  DATE                       : 2007-10-16
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization for structure component
!*                               in INTENT(OUT))
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

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'

        b%id = -1
    end subroutine
end module

module m1
use m, only : base
    type t (kt_1) ! kt_1=4
       integer, kind :: kt_1
        type(base(kt_1)) :: b1 = base(kt_1)(0) ! tcx: (kt_1) ! tcx: (kt_1)
        type(base(kt_1)), allocatable :: b2 ! tcx: (kt_1)
    end type

    contains

    subroutine abc (t1)
        type (t(4)), intent(out) :: t1 ! tcx: (4)
    end subroutine
end module

program ffinal513a5k
use m1
    type(t(4)), save :: t1 ! tcx: (4)

    t1%b1%id = 100
    allocate (t1%b2)

    call abc (t1)

    if (t1%b1%id /= 0) error stop 101_4

    if (allocated (t1%b2)) error stop 2_4

    print *, '2nd test'

    call abc (t1)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: t - added parameters (kt_1) to invoke with (4) / declare with (4) - 2 changes
