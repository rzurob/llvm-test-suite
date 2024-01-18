! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal534k
!*
!*  DATE                       : 2007-11-12 (original: 06/29/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (SAVE attribute on allocatable
!                               variables)
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

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent (in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(10:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)), intent(in) :: c(*) ! tcx: (4,*)

        print *, 'finalizeChildRank1'
    end subroutine
end module


program ffinal534k
    call test1

    print *, 'second call to test1'

    call test1

    print *, 'calling test2'

    call test2

    print *, 'end'
end

subroutine test1
use m
    class (base(4)), allocatable, save :: b1, b2(:), b3 (:,:) ! tcx: (4)

    if (allocated (b1)) then
        print *, 'b1 allocated'
    else
        allocate (child(4,20) :: b1) ! tcx: (4,20)
    end if

    if (allocated (b2)) then
        print *, 'b2 allocated, size = ', size (b2)
    else
        allocate (child(4,20) :: b2(2)) ! tcx: (4,20)
    end if

    if (allocated (b3)) then
        print *, 'b3 allocated, shape = ', shape (b3)
    else
        allocate (b3(2,2))
    end if
end subroutine

subroutine test2
use m
    class (base(4)), allocatable :: b1, b2(:), b3 (:,:) ! tcx: (4)

    allocate (child(4,20)::b2(2), b1) ! tcx: (4,20)

    allocate (b3(2,2))
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 5 changes
