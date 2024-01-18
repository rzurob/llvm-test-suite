! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514b2kl
!*
!*  DATE                       : 2007-11-07 (original: 06/30/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (allocatable function return result
!                               to be finalized (auto-deallocated) after use;
!                               test the call statement)
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

        procedure, nopass :: makeBaseObj => produceBaseAlloc
        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        procedure, nopass :: makeChildObj => produceChildAlloc
        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    type (base(4)) function produceBaseAlloc (id, l, u) ! tcx: (4)
        allocatable produceBaseAlloc (:)

        integer(4), intent(in) :: id, l, u

        allocate (produceBaseAlloc(l:u))

        produceBaseAlloc%id = id
    end function

    type (child(4,:)) function produceChildAlloc (id, name, l, u) ! tcx: (4,20)
        allocatable produceChildAlloc (:)
        integer(4), intent(in) :: id, l, u
        character(*), intent(in) :: name

        allocate (child(4,20):: produceChildAlloc(l:u)) ! tcx: child(4,20)

        produceChildAlloc%id = id
        produceChildAlloc%name = name
    end function

    subroutine printBase (b)
        type (base(4)), allocatable, intent(in) :: b(:) ! tcx: (4)

        if (allocated (b)) then
            print *, lbound(b,1), ubound(b,1)

            do i = lbound(b,1), ubound(b,1)
                print *, b(i)%id
            end do
        end if
    end subroutine

    subroutine printChild (c)
        type (child(4,:)), allocatable, intent(in) :: c(:) ! tcx: (4,*)

        if (allocated (c)) then
            print *, lbound(c,1), ubound(c,1)

            do i = lbound(c,1), ubound(c,1)
                print *, c(i)%id, c(i)%name
            end do
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)), intent(in) :: c(:) ! tcx: (4,*)

        print *, 'finalizeChildRank1'
    end subroutine
end module

program ffinal514b2kl
use m
    type (child(4,20)) c1 ! tcx: (4,20)

    call printChild (c1%makeChildObj (1, 'c1', 2, 3))

    call printBase (c1%makeBaseObj (10, 0, 2))

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 5 changes
