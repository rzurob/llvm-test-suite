! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : ffinal530kl
!*
!*  PROGRAMMER                 : David Forster (derived from ffinal530 by Jim Xia)
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
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name = 'default'

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    type container1 (kcontainer1_1) ! kcontainer1_1=4
       integer, kind :: kcontainer1_1
        type (base(kcontainer1_1)), allocatable :: data1 ! tcx: (kcontainer1_1)
        type (base(kcontainer1_1)), allocatable :: data2(:) ! tcx: (kcontainer1_1)
    end type

    type container2 (kcontainer2_1) ! kcontainer2_1=4
       integer, kind :: kcontainer2_1
        type (child(kcontainer2_1,:)), allocatable :: data1 ! tcx: (kcontainer2_1,:)
        type (child(kcontainer2_1,:)), allocatable :: data2(:) ! tcx: (kcontainer2_1,:)
    end type

    contains

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

program ffinal530kl
use m
    type (container1(4)) :: co1a, co1b ! tcx: (4)
    type (container2(4)) :: co2a, co2b ! tcx: (4)

    allocate (co1a%data1, co1a%data2(2:3), co1b%data1)

    co1b = co1a

    if ((lbound(co1b%data2,1) /= 2) .or. (ubound(co1b%data2,1) /= 3)) error stop 101_4

    print *, 'assignment the second time'

    co1b = co1a

    allocate (child(4,20) :: co2a%data1, co2a%data2 (0:2), co2b%data2(2))

    print *, 'the third assignment'

    co2b = co2a

    if ((lbound(co2b%data2,1) /= 0) .or. (ubound(co2b%data2,1) /= 2)) error stop 2_4

    print *, 'the last assignment'

    co2b = co2a

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 4 changes
! type: container1 - added parameters (kcontainer1_1) to invoke with (4) / declare with (4) - 1 changes
! type: container2 - added parameters (kcontainer2_1) to invoke with (4) / declare with (4) - 1 changes
