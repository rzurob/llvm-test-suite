! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal514b4kl
!*
!*  DATE                       : 2007-11-07 (original: 02/14/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the temps after the
!                               select type construct; test arrays)
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        integer(kbase_1) id

        contains

        final :: finalizeBase, finalizeBaseArray1
        procedure, nopass :: makeDataArray => produceBaseAlloc
    end type

    type, extends (base) :: child (lchild_1) ! lchild_1=10
       integer, len :: lchild_1
        character(lchild_1) name

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(in) :: b ! tcx: (8)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(8)), intent(in) :: b(:) ! tcx: (8)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,*)), intent(in) :: c ! tcx: (8,*)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child(8,*)), intent(in) :: c(:) ! tcx: (8,*)

        print *, 'finalizeChildArray1'
    end subroutine

    class (base(8)) function produceBaseAlloc (b, n) ! tcx: (8)
        allocatable :: produceBaseAlloc(:)
        class (base(8)), intent(in) :: b ! tcx: (8)
        intent(in) :: n

        allocate (produceBaseAlloc(n), source=b)
    end function
end module

program ffinal514b4kl
use m
    class (base(8)), pointer :: b1 ! tcx: (8)
    type (child(8,10)) :: c1 ! tcx: (8,10)

    nullify (b1)
    c1%id = 1
    c1%name = 'test'

    select type (x => b1%makeDataArray(c1, 2))
        type is (child(8,*)) ! tcx: (8,*)
            print *, x
        class default
            error stop 101_4
    end select

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 5 changes
! type: child - added parameters (lchild_1) to invoke with (8,10) / declare with (8,*) - 4 changes
