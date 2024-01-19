! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-01 (original: 02/10/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization not occurrs if the
!                               execution terminated due to error condition)
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
        integer(kbase_1), pointer :: id => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1), pointer :: name => null()

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        if (associated (b%id)) then
            print *, 'deallocating id'

            deallocate (b%id)
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

program ffinal506akl
use m
    class (base(4)), allocatable :: b1 ! tcx: (4)
    character(20), target :: c1

    allocate (child(4,20) :: b1) ! tcx: (4,20)

    allocate (b1%id, source= 100)

    select type (b1)
        type is (child(4,*)) ! tcx: (4,*)
            b1%name => c1
        class default
            error stop 101_4
    end select

    deallocate (b1)     !<-- this will fail and terminate the execution

    print *, 'end'      !<-- this line never prints out
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 2 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 3 changes
