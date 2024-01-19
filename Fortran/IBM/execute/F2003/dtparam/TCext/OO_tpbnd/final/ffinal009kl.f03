! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of the pointer returned
!                               from a function call)
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
        integer(kbase_1) :: id

        contains

        final :: finalizeBaseArray1, finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChildArray1
    end type

    contains

    function abc ()
        type(child(4,:)), pointer :: abc(:) ! tcx: (4,:)

        allocate (child(4,20)::abc(10)) ! tcx: child(4,20)

        abc%id = (/(i, i= 1, 10)/)
        abc%name = 'abc'
    end function

    subroutine finalizeBase (b)
        type(base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type(base(4)), intent(in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type(child(4,*)), intent(in) :: c(:) ! tcx: (4,*)

        print *, 'finalizeChildArray1'
    end subroutine
end module

program ffinal009kl
use m
    class (base(4)), pointer :: b1(:) ! tcx: (4)

    b1 => abc()

    if (size(b1) /= 10) error stop 101_4

    if (any (b1%id /= (/(j, j=1,10)/))) error stop 2_4

    deallocate (b1)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
