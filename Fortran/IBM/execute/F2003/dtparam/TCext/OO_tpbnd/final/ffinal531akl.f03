! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-11-11 (original: 06/22/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (automatic deallocation of the
!                               allocated allocatable subobjects in executable
!                               construct)
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
        procedure :: print => printBase
    end type

    type, extends (base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name

        contains

        procedure :: print => printChild
    end type

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), allocatable :: data(:) ! tcx: (kcontainer_1)
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine printData (d)
        type (container(4)), intent(in) :: d ! tcx: (4)

        if (allocated (d%data)) then
            print *, lbound(d%data, 1), ubound(d%data, 1)

            do i = lbound(d%data, 1), ubound(d%data, 1)
                call d%data(i)%print
            end do
        end if
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        print *, b%id, b%name
    end subroutine
end module

program ffinal531akl
use m
    type (child(4,20)) :: c1(3:5) ! tcx: (4,20)

    c1%id = (/3,4,5/)
    c1%name = (/'c1_3', 'c1_4', 'c1_5'/)

    print *, 'begin'

    call printData (container(4) (c1)) ! tcx: (4)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 2 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 2 changes
