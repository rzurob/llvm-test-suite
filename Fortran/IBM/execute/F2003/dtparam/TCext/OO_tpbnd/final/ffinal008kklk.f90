! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal008kklk
!*
!*  DATE                       : 2007-10-31 (original: 02/09/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (parent components finalization in
!                               step 3; will start the finalization chain)
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
    type A (kA_1) ! kA_1=4
       integer, kind :: kA_1
        integer(kA_1), pointer :: id(:)

        contains

        final :: finalizeA
    end type

    type base

    end type

    type, extends (base) :: child (kchild_1,lchild_1) ! kchild_1,lchild_1=4,20
       integer, kind :: kchild_1
       integer, len :: lchild_1
        type (A(kchild_1)) a1 ! tcx: (kchild_1)
        character(lchild_1), pointer :: name(:)

        contains

        final :: finalizeChild
    end type

    type, extends (child) :: gen3 (kGen3) ! kGen3=8
       integer, kind :: kGen3
    end type

    contains

    subroutine finalizeA (a1)
        type (A(4)), intent(inout) :: a1 ! tcx: (4)

        if (associated (a1%id)) then
            print *, 'deallocating id'

            deallocate (a1%id)
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(inout) :: c ! tcx: (4,*)

        if (associated (c%name)) then
            print *, 'deallocating name'

            deallocate (c%name)
        end if
    end subroutine
end module

program ffinal008kklk
use m
    class (base), pointer :: b1, b2(:)

    allocate (gen3(4,20,8):: b1, b2(0:1)) ! tcx: (4,20,8)

    select type (b1)
        class is (child(4,*)) ! tcx: (4,*)
            allocate (b1%a1%id(2), source= (/1, 2/))
            allocate (b1%name(2), source=(/'abc', 'xyz'/))
        class default
            error stop 101_4
    end select

    print *, 'test 1'
    deallocate (b1)

    select type (b2)
        type is (gen3(4,*,8)) ! tcx: (4,*,8)
            allocate (b2(0)%a1%id(2), source= (/10, 20/))
            allocate (b2(1)%a1%id(2), source=(/-1, -2/))
            allocate (b2(0)%name(0:1), source= (/'xlf', 'com'/))
            allocate (b2(1)%name(0:1), source= (/'xlf', 'com'/))
        class default
            error stop 2_4
    end select

    print *, 'test 2'

    deallocate (b2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (kchild_1,lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
! type: gen3 - added parameters (kGen3,8) to invoke with (4,20,8) / declare with (4,*,8) - 2 changes
! type: gen3 - added parameters (kGen3) to invoke with (4,20,8) / declare with (4,*,8) - 2 changes
