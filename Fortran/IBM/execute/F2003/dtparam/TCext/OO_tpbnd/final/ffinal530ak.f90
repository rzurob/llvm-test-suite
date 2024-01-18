! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal530ak
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal530a by Jim Xia)
!*  DATE                       : 2007-11-11 (original: 06/21/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound 
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : final sub (allocated allocatable subojects in
!                               intrinsic assignment: NOTE it is possible to
!                               observe the order change of the finalization of
!                               two allocatable components, data1 and data2.  If
!                               that happens; update the expected results.)
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
        integer(kbase_1) :: id = -1

        contains

        final :: finalizeBase, finalizeBaseRank1
        procedure :: print => printBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'default'

        contains

        final :: finalizeChild, finalizeChildRank1
        procedure :: print => printChild
    end type

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), allocatable :: data1 ! tcx: (kcontainer_1)
        class (base(kcontainer_1)), allocatable :: data2(:) ! tcx: (kcontainer_1)
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent (in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        print *, b%id, b%name
    end subroutine

    subroutine finalizeChild (c)
        type (child(4,*)) :: c ! tcx: (4,20)

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4,*)) :: c(:) ! tcx: (4,20)

        print *, 'finalizeChildRank1'
    end subroutine

    subroutine finalizeBase(b)
        type (base(4)), intent (in) :: b ! tcx: (4)

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base(4)), intent (in) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program ffinal530ak
use m
    type (container(4)) :: co1, co2 ! tcx: (4)

    allocate (co1%data1, source=child(4,20)(1, 'co1.data1')) ! tcx: (4,20)
    allocate (co1%data2 (2:3), source=child(4,20)(2, 'co1.data2')) ! tcx: (4,20)
    allocate (child(4,20) :: co2%data1) ! tcx: (4,20)
    allocate (co2%data2(5))

    print *, 'first assignment'

    co2 = co1

    if ((lbound(co2%data2,1) /= 2) .or. (ubound(co2%data2,1) /= 3)) error stop 101_4

    call co2%data1%print

    call co2%data2(2)%print
    call co2%data2(3)%print

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 6 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 1 changes
