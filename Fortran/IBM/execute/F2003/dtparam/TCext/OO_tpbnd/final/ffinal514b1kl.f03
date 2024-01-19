! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-07 (original: 06/30/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (allocatable function return result
!                               is automatically deallocated after use)
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

        procedure, nopass, non_overridable :: makeObj => produceBaseAlloc
        final :: finalizeBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        final :: finalizeChild
    end type

    contains

    function produceBaseAlloc (id, name)
        class (base(4)), allocatable :: produceBaseAlloc ! tcx: (4)

        integer(4), intent(in), optional :: id
        character(*), intent(in), optional :: name

        if (.not. present (id)) then
            return
        else
            if (.not. present (name)) then
                allocate (produceBaseAlloc)
                produceBaseAlloc%id = id
            else
                allocate (produceBaseAlloc, source=child(4,20)(id, name)) ! tcx: (4,20)
            end if
        end if
    end function

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)), intent(in) :: c ! tcx: (4,*)

        print *, 'finalizeChild'
    end subroutine
end module

program ffinal514b1kl
use m
    class (base(4)), allocatable :: b1 ! tcx: (4)

    print *, allocated (b1%makeObj())

    if (allocated (b1%makeObj (name='null'))) error stop 101_4

    print *, allocated (b1%makeObj (10))

    if (.not. allocated (b1%makeObj (-1, 'temp'))) error stop 2_4

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
